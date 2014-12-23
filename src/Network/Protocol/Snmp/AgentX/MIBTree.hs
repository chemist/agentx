{-# LANGUAGE DeriveDataTypeable #-}
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
, printTree
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
)
where

import Data.Typeable (Typeable)
import Data.Monoid ((<>))
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.List (stripPrefix, sort)
import Control.Monad.State (StateT, get, put, liftIO, modify)
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception, throw)

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Protocol (SearchRange(..))

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
  , val ::Value 
  , upd :: Update
  } deriving (Eq)

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
            | Read (IO Value)
            | ReadWrite (IO Value) (Value -> IO ())
            
instance Eq Update where
    _ == _ = True

instance Show Update where
    show Fixed = "fixed"
    show (Read _) = "read-only"
    show (ReadWrite _ _) = "read-write"

newtype UTree = UTree (IO [MIB])

instance Show UTree where
    show _ = "dynamic"

instance Eq UTree where
    _ == _ = True
                
data MIBTree = Root OID Name MIBTree
             | Node Integer Parent Name MIBTree MIBTree (Maybe UTree)
             | Leaf Integer Parent Name MIBTree Value Update
             | Empty
             deriving (Show, Eq)

singleton :: MIB -> MIBTree 
singleton m = singleton' (oid m, m)
  where
    singleton' :: (OID, MIB) -> MIBTree
    singleton' ([], _) = Empty
    singleton' ([_], Object _ i p n u) = Node i p n Empty Empty u
    singleton' ((x:xs), mm@(Object _ _ p n u)) = Node x  p n Empty (singleton' (xs, mm)) u
    singleton' ([_], ObjectType _ i p n v u) = Leaf i p n Empty v u
    singleton' ((x:xs), mm@(ObjectType _ _ p n _ _)) = Node x p n Empty (singleton' (xs, mm)) Nothing

insert :: MIBTree -> MIBTree -> MIBTree
insert a Empty = a
insert Empty a = a
insert (Node i p n next link u) x@(Node i1 _ _ next1 link1 _)
  | i == i1 = Node i p n (next `insert` next1) (link `insert` link1) u
  | otherwise = Node i p n (next `insert` x) link u
insert (Node i p n next link u) x@(Leaf{}) = Node i p n (next `insert` x) link u
insert (Leaf i p n next v u) x = Leaf i p n (next `insert` x) v u 
insert _ _ = error "bad insert usage"
   
fromList :: [MIB] -> MIBTree
fromList [] = Empty
fromList y@(x:_)
    | oid x == [] = foldl1 insert . map singleton . sort . makeOid $ y
    | otherwise = dropRoot x (foldl1 insert . map singleton . sort . makeOid $ y)
    where
      dropRoot a z@(Node _ _ n _ link _)
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
toList' (o, Node i p n next link u) = Object (o <> [i]) i p n u : toList' (o, next) <> toList' (o <> [i], link)
toList' (o, Leaf i p n next v u) = ObjectType (o <> [i]) i p n v u : toList' (o, next)
toList' (_, _) = []

printTree :: MIBTree -> IO ()
printTree f = putStr $ unlines $ drawLevel f
  where
    drawLevel Empty = []
    drawLevel (Root o n link) = ("Root " <> show o <> " " <> n <> " " ) : drawSubtree Empty link
    drawLevel (Node i _ n next link _) = ("Object " <> show i <> " " <> n <> " " ) : (drawSubtree next link)
    drawLevel (Leaf i _ n next v _) = ("ObjectType " <> show i <> " " <> n <> " " <> show v <> " " ) : (drawSubtree next Empty)
    
    drawSubtree next link = (shift "`- " " | " (drawLevel link)) <> drawLevel next

    shift first rest = zipWith (++) (first : repeat rest)

mkModule :: OID -> Parent -> Name -> MIB
mkModule [] _ _ = error "oid cant be empty"
mkModule o p n  = Object o (last o) p n Nothing

mkObject :: Integer -> Parent -> Name -> Maybe UTree -> MIB
mkObject = Object []

mkObjectType :: Integer -> Parent -> Name -> Value -> Update -> MIB
mkObjectType = ObjectType []

---------------------------------------------------------------------------------------------------------
-- zipper
---------------------------------------------------------------------------------------------------------

data Move = Next MIBTree | Level MIBTree deriving (Show)

type Moving = [Move]

type Zipper = (MIBTree, Moving)

toZipper :: MIBTree -> Zipper 
toZipper t = (t, [])

attach :: MIBTree -> Zipper -> Zipper 
attach t (_, bs) = (t, bs)

goNext :: Zipper -> Maybe Zipper 
goNext (Empty, _) = Nothing
goNext (Node _ _ _ Empty _ _, _) = Nothing
goNext (Leaf _ _ _ Empty _ _, _) = Nothing
goNext (Root _ _ _, _) = Nothing
goNext (Leaf i p n next v    u, bs) = Just (next, Next (Leaf i p n Empty v u):bs)
goNext (Node i p n next link u, bs) = Just (next, Next (Node i p n Empty link u):bs)

goLevel :: Zipper -> Maybe Zipper 
goLevel (Empty, _) = Nothing
goLevel (Leaf{}, _) = Nothing
goLevel (Node _ _ _ _ Empty _, _) = Nothing
goLevel (Node i p n next link u, bs) = Just (link, Level (Node i p n next Empty u):bs)
goLevel (Root o n link, bs) = Just (link, Level (Root o n Empty):bs)

goBack :: Zipper -> Maybe Zipper 
goBack (_, []) = Nothing
goBack (t, Next (Leaf i p n Empty v u):bs) = Just (Leaf i p n t v u, bs)
goBack (t, Next (Node i p n Empty link u):bs) = Just (Node i p n t link u, bs)
goBack (t, Level (Node i p n next Empty u):bs) = Just (Node i p n next t u, bs)
goBack (t, Level (Root o n Empty):[]) = Just (Root o n t, [])
goBack _ = Nothing

goUp :: Zipper -> Maybe Zipper 
goUp (_, []) = Nothing
goUp (t, Next (Leaf i p n Empty v u):bs) = goUp (Leaf i p n t v u, bs)
goUp (t, Next (Node i p n Empty link u):bs) = goUp (Node i p n t link u, bs)
goUp (t, Level (Node i p n next Empty u):bs) = Just (Node i p n next t u, bs)
goUp (t, Level (Root o n Empty):[]) = Just (Root o n t, [])
goUp _ = Nothing

top :: Zipper -> Zipper
top (t,[]) = (t,[])  
top z = top (fromJust $ goBack z)

getFocus :: Zipper -> MIB
getFocus z = fromTree (fst z) (getOid z)
  where
  fromTree Empty _ = undefined
  fromTree (Root o n _) _ = Object o (last o) "" n Nothing
  fromTree (Node i p n _ _ u) o = Object o i p n u
  fromTree (Leaf i p n _ v u) o = ObjectType o i p n v u

type Base = StateT Zipper IO

update :: Maybe Value -> MIB -> Base MIB
update mv m = case upd m of
                Fixed -> return m
                Read fun -> setValue m <$> liftIO fun 
                ReadWrite reread write -> do
                    let newMib = maybe m (setValue m) mv
                    liftIO $ write (val newMib)
                    setValue newMib <$> liftIO reread

updateTree :: Base ()
updateTree = do
    c <- getFocus <$> get
    case isDynamic c of
       Just (UTree fun) -> do
           n <- liftIO fun 
           modify $ attach $ fromList n
       Nothing -> return ()
    where
    isDynamic (Object _ _ _ _ x) = x
    isDynamic _ = Nothing

setValue :: MIB -> Value -> MIB
setValue (ObjectType o i p n _ u) v = ObjectType o i p n v u
setValue _ _ = throw CantReadValue

goClosest :: OID -> Base ()
goClosest xs = do
    modify top
    o <- getFocus <$> get
    modify $ fromJust . goLevel
    walk' $ fromMaybe [] (stripPrefix (oid o) xs)
    where
        walk' :: OID -> Base ()
        walk' [] = return ()
        walk' (x:ys) = do
            c <- int . getFocus <$> get
            l <- hasLevel
            n <- hasNext
            case (x == c, l, n) of
                 (True, True, _) -> do
                     updateTree
                     unless (ys == []) $ modify $ fromJust . goLevel
                     walk' ys
                 (False, _, True) -> do
                     modify $ fromJust . goNext
                     walk' (x:ys)
                 _ -> return ()

findOne :: OID -> Base MIB
findOne xs = do
    goClosest xs
    o <- getOid <$> get
    t <- isObjectType
    case (o == xs, t) of
         (True, True) -> update Nothing =<< getFocus <$> get
         (True, False) -> return $ ObjectType o (last o) "" "" NoSuchObject Fixed
         _ -> return $ ObjectType o (last o) "" "" NoSuchInstance Fixed

findMany :: [OID] -> Base [MIB]
findMany [] = return []
findMany (x:xs) = (:) <$> findOne x <*> findMany xs

isObjectType :: Base Bool
isObjectType = do
    f <- getFocus <$> get
    case f of
         ObjectType{} -> return True
         _ -> return False

-- focus :: Base ()
-- focus = liftIO . print =<< getFocus <$> get

getOid :: Zipper -> OID
getOid (Root o _ _, []) = o
getOid z = foldl fun [] (snd z) <> [getInt (fst z)]
  where
  fun xs Next{}= xs
  fun xs (Level (Node i _ _ _ _ _)) = i:xs
  fun xs (Level (Leaf i _ _ _ _ _)) = i:xs
  fun xs (Level (Root o _ _)) = o <> xs
  fun _ _ = error "bad zipper"

getInt :: MIBTree -> Integer
getInt (Node i _ _ _ _ _) = i
getInt (Leaf i _ _ _ _ _) = i
getInt _ = error "bad getInt"

findNext :: SearchRange -> Base MIB
findNext s@(SearchRange (start, end, True)) = do
    goClosest start
    o <- getFocus <$> get
    t <- isObjectType
    if oid o == start && t
       then inRange s <$> update Nothing o
       else inRange s <$> findNext (SearchRange (start, end, False))
findNext s@(SearchRange (start, _end, False)) = do
    goClosest start
    l <- hasLevel
    n <- hasNext
    case (l, n) of
         (True, _) -> do
             modify $ fromJust . goLevel
             m <- findClosestObject' False start
             inRange s <$> update Nothing m
         (False, True) -> do
             modify $ fromJust . goNext
             m <- findClosestObject' False start
             inRange s <$> update Nothing m
         (False, False) -> do
             modify $ fromJust . goUp
             m <- findClosestObject' True start
             inRange s <$> update Nothing m

inRange :: SearchRange -> MIB -> MIB
inRange (SearchRange (from, to, _)) m = 
  if from <= oid m && oid m < to 
     then m 
     else (ObjectType from 0 "" "" EndOfMibView Fixed)

findClosestObject' :: Bool -> OID -> Base MIB
findClosestObject' back oid' = do
    t <- isObjectType
    l <- (if back then not else id) <$> hasLevel
    n <- hasNext
    case (t, l, n) of
         (True, _, _) -> getFocus <$> get
         (False, True, _) -> do
             modify (fromJust . goLevel)
             findClosestObject' False oid'
         (False, False, True) -> do
             modify (fromJust . goNext) 
             findClosestObject' False oid'
         (False, False, False) -> do 
             st <- get
             case (goUp st) of
                  Just ust -> put ust >> findClosestObject' True oid'
                  Nothing -> return $ ObjectType oid' 0 "" "" EndOfMibView Fixed

hasLevel :: StateT Zipper IO Bool
hasLevel = isJust . goLevel <$> get

hasNext :: StateT Zipper IO Bool
hasNext = isJust . goNext <$> get

