{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.Protocol.Snmp.AgentX.MIBTree 
( MIB
, Base
, mkModule
, mkObject
, mkObjectType
, toList
, fromList
, Update(..)
, UTree(..)
, MIBTree
, toZipper
, Zipper
, findOne
, findMany
, findNext
, MIBException(..)
, oid
, val
, name
, upd
, isWritable
, ContextedValue
, defaultContext
, isObjectType
, BaseST(..)
)
where

import Data.Typeable (Typeable)
import Data.Monoid ((<>))
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.List (stripPrefix, sort)
import Control.Monad.State.Strict (StateT, get, put, liftIO)
import Control.Monad
import Control.Concurrent.MVar
import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception, throw)
import qualified Data.Map.Strict as Map
import qualified Data.Label as DL

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Packet (SearchRange, Context, include, startOID, endOID, SearchRange, CommitError, TestError, UndoError)

data MIB = Object 
  { oid :: OID
  , int :: Integer
  , parent :: Parent 
  , name  :: Name 
  , _updu :: (Maybe UTree)
  }      | ObjectType 
  { oid :: OID
  , int :: Integer
  , parent :: Parent 
  , name :: Name 
  , val :: ContextedValue
  , upd :: Update
  } deriving Eq

type ContextedValue = Map.Map Context Value

defaultContext :: Value -> ContextedValue
defaultContext v = Map.singleton "" v

instance Ord MIB where
    compare x y = compare (oid x) (oid y)

instance Show MIB where
    show (Object o i p n u) = "Object " <> oidToString o <> " " <> show i <> " " <> show p <> " " <> show n <> " " <> show u
    show (ObjectType o i p n v u) = "ObjectType " <> oidToString o <> " " <> show i <> " " <> show p <> " " <> show n <> " " <> show v <> " " <> show u

oidToString :: OID -> String
oidToString [] = ""
oidToString xs = init $ foldr (\a b -> show a <> "." <> b) "" xs

type Parent = String
type Name = String

data Update = Fixed
            | Read 
              { readAIO :: IO ContextedValue }
            | ReadWrite 
              { readAIO        :: IO ContextedValue
              , commitSetAIO   :: Maybe Context -> Value -> IO CommitError 
              , testSetAIO     :: Maybe Context -> Value -> IO TestError
              , undoSetAIO     :: Maybe Context -> Value -> IO UndoError
              }
            
instance Eq Update where
    _ == _ = True

instance Show Update where
    show Fixed = "fixed"
    show (Read _) = "read-only"
    show (ReadWrite _ _ _ _) = "read-write"

newtype UTree = UTree (IO [MIB])

instance Show UTree where
    show _ = "dynamic"

instance Eq UTree where
    _ == _ = True
                
data MIBTree = Root OID Name MIBTree
             | Node Integer Name MIBTree MIBTree (Maybe UTree)
             | Leaf Integer Name MIBTree ContextedValue Update
             | Empty
             deriving (Eq)

singleton :: MIB -> MIBTree 
singleton m = singleton' (oid m, m)
  where
    singleton' :: (OID, MIB) -> MIBTree
    singleton' ([], _) = Empty
    singleton' ([_], Object _ i _ n u) = Node i n Empty Empty u
    singleton' ((x:xs), mm@(Object _ _ _ n u)) = Node x n Empty (singleton' (xs, mm)) u
    singleton' ([_], ObjectType _ i _ n v u) = Leaf i n Empty v u
    singleton' ((x:xs), mm@(ObjectType _ _ _ n _ _)) = Node x n Empty (singleton' (xs, mm)) Nothing

insert :: MIBTree -> MIBTree -> MIBTree
insert a Empty = a
insert Empty a = a
insert (Node i n next link u) x@(Node i1 _ next1 link1 _)
  | i == i1 = Node i n (next `insert` next1) (link `insert` link1) u
  | otherwise = Node i n (next `insert` x) link u
insert (Node i n next link u) x@(Leaf{}) = Node i n (next `insert` x) link u
insert (Leaf i n next v u) x = Leaf i n (next `insert` x) v u 
insert _ _ = error "bad insert usage"
   
fromList :: [MIB] -> MIBTree
fromList [] = Empty
fromList y@(x:_)
    | oid x == [] = foldl1 insert . map singleton . sort . makeOid $ y
    | otherwise = dropRoot x (foldl1 insert . map singleton . sort . makeOid $ y)
    where
      dropRoot a z@(Node  _ n _ link _)
        | n == name a = dropRoot a link
        | otherwise = Root (oid a) (name a) z
      dropRoot _ _ = error "bad dropParted"

data MIBException = BadTempMib
                  | CantReadValue
                  | NotDescribedUpdate
                  | HasSuchObject 
                  | EndOfMib
                  deriving (Show, Typeable)

instance Exception MIBException

makeOid :: [MIB] -> [MIB]
makeOid [] = []
makeOid (ObjectType{} : _) = error "makeOid: first record cant be ObjectType"
makeOid (Object o i p n u : xs) 
  | o == [] = Object [i] i p n u :  mkOid' [(n, [i])] xs
  | otherwise = Object o i p n u : mkOid' [(n, o)] xs
  where
    mkOid' :: [(Parent, OID)] -> [MIB] -> [MIB]
    mkOid' _ [] = []
    mkOid' base (y:ys) =
        let Just prev = lookup (parent y) base
            newbase = (name y, prev <> [int y]) : base
        in addOid prev y : mkOid' newbase ys
    addOid :: OID -> MIB -> MIB
    addOid o' (Object _ i' p' n' u') = Object (o' <> [i']) i' p' n' u'
    addOid o' (ObjectType _ i' p' n' v' u') = ObjectType (o' <> [i']) i' p' n' v' u'
    
toList :: MIBTree -> [MIB]
toList Empty  = []
toList (Root oi n l) = Object oi (last oi) "" n Nothing : toList' (oi, l)
toList x = toList' ([], x)

toList' :: (OID, MIBTree) -> [MIB]
toList' (o, Node i n next link u) = Object (o <> [i]) i "" n u : toList' (o, next) <> toList' (o <> [i], link)
toList' (o, Leaf i n next v u) = ObjectType (o <> [i]) i "" n v u : toList' (o, next)
toList' (_, _) = []

instance Show MIBTree where
    show f = unlines $ drawLevel f
      where
        drawLevel Empty = []
        drawLevel (Root o n link) = ("Root " <> show o <> " " <> n <> " " ) : drawSubtree Empty link
        drawLevel (Node i n next link _) = ("Object " <> show i <> " " <> n <> " " ) : (drawSubtree next link)
        drawLevel (Leaf i n next v _) = ("ObjectType " <> show i <> " " <> n <> " " <> show v <> " " ) : (drawSubtree next Empty)
        
        drawSubtree next link = (shift "`- " " | " (drawLevel link)) <> drawLevel next
    
        shift first rest = zipWith (++) (first : repeat rest)

instance Show Zipper where
    show (f,_) = show f

mkModule :: OID -> Parent -> Name -> MIB
mkModule [] _ _ = error "oid cant be empty"
mkModule o p n  = Object o (last o) p n Nothing

mkObject :: Integer -> Parent -> Name -> Maybe UTree -> MIB
mkObject = Object []

mkObjectType :: Integer -> Parent -> Name -> ContextedValue -> Update -> MIB
mkObjectType = ObjectType []

---------------------------------------------------------------------------------------------------------
-- zipper
---------------------------------------------------------------------------------------------------------

data Move = Next MIBTree | Level MIBTree

type Moving = [Move]

type Zipper = (MIBTree, Moving)

toZipper :: MIBTree -> Zipper 
toZipper t = (t, [])

attach :: MIBTree -> Zipper -> Zipper 
attach t (_, bs) = (t, bs)

goNext :: Zipper -> Maybe Zipper 
goNext (Empty, _) = Nothing
goNext (Node _ _ Empty _ _, _) = Nothing
goNext (Leaf _ _ Empty _ _, _) = Nothing
goNext (Root _ _ _, _) = Nothing
goNext (Leaf i n next v    u, bs) = Just (next, Next (Leaf i n Empty v u):bs)
goNext (Node i n next link u, bs) = Just (next, Next (Node i n Empty link u):bs)

goLevel :: Zipper -> Maybe Zipper 
goLevel (Empty, _) = Nothing
goLevel (Leaf{}, _) = Nothing
goLevel (Node _ _ _ Empty _, _) = Nothing
goLevel (Node i n next link u, bs) = Just (link, Level (Node i n next Empty u):bs)
goLevel (Root o n link, bs) = Just (link, Level (Root o n Empty):bs)

goBack :: Zipper -> Maybe Zipper 
goBack (_, []) = Nothing
goBack (t, Next (Leaf i n Empty v u):bs) = Just (Leaf i n t v u, bs)
goBack (t, Next (Node i n Empty link u):bs) = Just (Node i n t link u, bs)
goBack (t, Level (Node i n next Empty u):bs) = Just (Node i n next t u, bs)
goBack (t, Level (Root o n Empty):[]) = Just (Root o n t, [])
goBack _ = Nothing

goUp :: Zipper -> Maybe Zipper 
goUp (_, []) = Nothing
goUp (t, Next (Leaf i n Empty v u):bs) = goUp (Leaf i n t v u, bs)
goUp (t, Next (Node i n Empty link u):bs) = goUp (Node i n t link u, bs)
goUp (t, Level (Node i n next Empty u):bs) = Just (Node i n next t u, bs)
goUp (t, Level (Root o n Empty):[]) = Just (Root o n t, [])
goUp _ = Nothing

top :: Zipper -> Zipper
top (t,[]) = (t,[])  
top z = top (fromJust $ goBack z)

getFocus :: Zipper -> MIB
getFocus z@(x, _) = fromTree x (getOid z)
  where
  fromTree Empty _ = error "getFocus"
  fromTree (Root o n _) _ = Object o (last o) "" n Nothing
  fromTree (Node i n _ _ u) o = Object o i "" n u
  fromTree (Leaf i n _ v u) o = ObjectType o i "" n v u

type Base = StateT BaseST IO

data BaseST = BST 
  { zipper :: Zipper 
  , toRegister :: MVar [MIB] 
  , toUnregister :: MVar [MIB]
  }

getZip :: Base Zipper
getZip = zipper <$> get

putZip :: Zipper -> Base ()
putZip z = do
    s <- get
    put $ s { zipper = z }

modifyZip :: (Zipper -> Zipper) -> Base ()
modifyZip f = do
    s <- get
    put $ s { zipper = f (zipper s) }

update :: MIB -> Base MIB
update m = case upd m of
                Fixed -> return m
                Read fun -> setValue m <$> liftIO fun 
                rw@ReadWrite{} -> setValue m <$> liftIO (readAIO rw)

updateTree :: Base ()
updateTree = do
    c <- getFocus <$> getZip
    case isDynamic c of
       Just (UTree fun) -> do
           n <- liftIO fun 
           modifyZip $ attach $ fromList n
       Nothing -> return ()
    where
    isDynamic (Object _ _ _ _ x) = x
    isDynamic _ = Nothing

setValue :: MIB -> ContextedValue -> MIB
setValue (ObjectType o i p n _ u) v = ObjectType o i p n v u
setValue _ _ = throw CantReadValue

goClosest :: OID -> Base ()
goClosest xs = do
    modifyZip top
    o <- getFocus <$> getZip
    modifyZip $ fromJust . goLevel
    walk' $ fromMaybe [] (stripPrefix (oid o) xs)
    where
        walk' :: OID -> Base ()
        walk' [] = return ()
        walk' (x:ys) = do
            c <- int . getFocus <$> getZip
            l <- hasLevel
            n <- hasNext
            case (x == c, l, n) of
                 (True, True, _) -> do
                     updateTree
                     unless (ys == []) $ modifyZip $ fromJust . goLevel
                     walk' ys
                 (False, _, True) -> do
                     modifyZip $ fromJust . goNext
                     walk' (x:ys)
                 _ -> return ()

findOne :: OID -> Base MIB
findOne xs = do
    goClosest xs
    o <- getOid <$> getZip
    t <- isFocusObjectType
    case (o == xs, t) of
         (True, True) -> update =<< getFocus <$> getZip
         (True, False) -> return $ ObjectType o (last o) "" "" (defaultContext NoSuchObject) Fixed
         _ -> return $ ObjectType xs (last xs) "" "" (defaultContext NoSuchInstance) Fixed

findMany :: [OID] -> Base [MIB]
findMany [] = return []
findMany (x:xs) = (:) <$> findOne x <*> findMany xs

isFocusObjectType :: Base Bool
isFocusObjectType = isObjectType . getFocus <$> getZip

isObjectType :: MIB -> Bool
isObjectType ObjectType{} = True
isObjectType _ = False

isWritable :: MIB -> Bool
isWritable (ObjectType _ _ _ _ _ (ReadWrite _ _ _ _)) = True
isWritable _ = False

getOid :: Zipper -> OID
getOid (Root o _ _, []) = o
getOid (t, z) = foldl fun [] z <> [getInt t]
  where
  fun xs Next{}= xs
  fun xs (Level (Node i _ _ _ _)) = i:xs
  fun xs (Level (Leaf i _ _ _ _)) = i:xs
  fun xs (Level (Root o _ _)) = o <> xs
  fun _ _ = error "bad zipper"

getInt :: MIBTree -> Integer
getInt (Node i _ _ _ _) = i
getInt (Leaf i _ _ _ _) = i
getInt _ = error "bad getInt"

findNext :: SearchRange -> Base MIB
findNext s 
  | DL.get include s = do
    goClosest (DL.get startOID s)
    o <- getFocus <$> getZip
    t <- isFocusObjectType
    if oid o == (DL.get startOID s) && t
       then inRange s <$> update o
       else inRange s <$> findNext (DL.set include False s)
  | otherwise = do
    goClosest (DL.get startOID s)
    l <- hasLevel
    n <- hasNext
    case (l, n) of
         (True, _) -> do
             modifyZip $ fromJust . goLevel
             m <- findClosestObject' False (DL.get startOID s)
             inRange s <$> update m
         (False, True) -> do
             modifyZip $ fromJust . goNext
             m <- findClosestObject' False (DL.get startOID s)
             inRange s <$> update m
         (False, False) -> do
             modifyZip $ fromJust . goUp
             m <- findClosestObject' True (DL.get startOID s)
             inRange s <$> update m

inRange :: SearchRange -> MIB -> MIB
inRange s m = 
  if (DL.get startOID s) <= oid m && oid m < (DL.get endOID s)
     then m 
     else (ObjectType (DL.get startOID s) 0 "" "" (defaultContext EndOfMibView) Fixed)

findClosestObject' :: Bool -> OID -> Base MIB
findClosestObject' back oid' = do
    t <- isFocusObjectType
    l <- (if back then not else id) <$> hasLevel
    n <- hasNext
    case (t, l, n) of
         (True, _, _) -> getFocus <$> getZip
         (False, True, _) -> do
             modifyZip (fromJust . goLevel)
             findClosestObject' False oid'
         (False, False, True) -> do
             modifyZip (fromJust . goNext) 
             findClosestObject' False oid'
         (False, False, False) -> do 
             st <- getZip
             case (goUp st) of
                  Just ust -> putZip ust >> findClosestObject' True oid'
                  Nothing -> return $ ObjectType oid' 0 "" "" (defaultContext EndOfMibView) Fixed

hasLevel :: Base Bool
hasLevel = isJust . goLevel . zipper <$> get

hasNext :: Base Bool
hasNext = isJust . goNext . zipper <$> get

