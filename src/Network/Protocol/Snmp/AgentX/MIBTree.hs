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
, iso
, org
, dod
, internet
, private
, enterprise
, toList
, fromList
, insert
, printTree
, Update(..)
, UTree(..)
, MIBTree(..)
, toZipper
, Zipper
, goNext
, goLevel
, goUp
, attach
, getFocus
, focus
, top
, fromListWithFirstAsBase
, findOne
, findMany
, findNext
, MIBException(..)
, goClosest
, Items(..)
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

data MIB =  Object OID Integer Parent Name (Maybe UTree)
         | ObjectType OID Integer Parent Name Value Update
         deriving (Eq)

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

class Items a where
    parent :: a -> Parent
    name   :: a -> Name
    int    :: a -> Integer
    oid    :: a -> OID
    val    :: a -> Value
    upd    :: a -> Update

instance Items MIB where
    val (ObjectType _ _ _ _ v _) = v
    val _ = NoSuchObject
    upd (ObjectType _ _ _ _ _ u) = u
    upd _ = Fixed
    parent (Object _ _ x _ _) = x
    parent (ObjectType _ _ x _ _ _) = x
    name (Object _ _ _ x _) = x
    name (ObjectType _ _ _ x _ _) = x
    int (Object _ x _ _ _) = x
    int (ObjectType _ x _ _ _ _) = x
    oid (Object x _ _ _ _) = x
    oid (ObjectType x _ _ _ _ _) = x

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
                
data MIBTree = Node OID Integer Parent Name MIBTree MIBTree (Maybe UTree)
             | Leaf OID Integer Parent Name MIBTree Value Update
             | Empty
             deriving (Show, Eq)

singleton :: MIB -> MIBTree 
singleton m = singleton' (oid m, m)
  where
    singleton' :: (OID, MIB) -> MIBTree
    singleton' ([], _) = Empty
    singleton' ([_], Object o i p n u) = Node o i p n Empty Empty u
    singleton' ((x:xs), mm@(Object o _ p n u)) = Node o x  p n Empty (singleton' (xs, mm)) u
    singleton' ([_], ObjectType o i p n v u) = Leaf o i p n Empty v u
    singleton' ((x:xs), mm@(ObjectType o _ p n _ _)) = Node o x p n Empty (singleton' (xs, mm)) Nothing

insert :: MIBTree -> MIBTree -> MIBTree
insert a Empty = a
insert Empty a = a
insert (Node o i p n next link u) x@(Node _ i1 _ _ next1 link1 _)
  | i == i1 = Node o i p n (next `insert` next1) (link `insert` link1) u
  | otherwise = Node o i p n (next `insert` x) link u
insert (Node o i p n next link u) x@(Leaf{}) = Node o i p n (next `insert` x) link u
insert (Leaf o i p n next v u) x = Leaf o i p n (next `insert` x) v u 
   

-- iso(1) org(3) dod(6) internet(1) mgmt(2) mib-2(1)
iso :: MIB
iso = Object [1] 1 "" "iso" Nothing

org :: MIB
org = Object [1,3] 3 "iso" "org" Nothing

dod :: MIB
dod = Object [1,3,6] 6 "org" "dod" Nothing

internet :: MIB 
internet = Object [1,3,6,1] 1 "dod" "internet" Nothing

private :: MIB  
private = Object [1,3,6,1,4] 4 "internet" "private" Nothing

enterprise :: MIB 
enterprise = Object [1,3,6,1,4,1] 1 "private" "enterprise" Nothing

fromList :: [MIB] -> MIBTree
fromList = foldl1 insert . map singleton . sort . makeOid

data MIBException = BadTempMib
                  | CantReadValue
                  | NotDescribedUpdate
                  | HasSuchObject 
                  | EndOfMib
                  deriving (Show, Typeable)

instance Exception MIBException

makeOid :: [MIB] -> [MIB]
makeOid xs = makeOid' [] xs
  where
    makeOid' :: [(Name, OID)] -> [MIB] -> [MIB]
    makeOid' _ [] = []
    makeOid' _ (x@(Object [1] 1 "" "iso" _):ys) = x : makeOid' [("iso", [1])] ys
    makeOid' base (x:ys) =
       let Just prev = lookup (parent x) base 
           newbase = (name x, prev <> [int x]) : base
       in addOid prev x : makeOid' newbase ys

addOid :: OID -> MIB -> MIB
addOid o (Object _ i p n u) = Object (o <> [i]) i p n u
addOid o (ObjectType _ i p n v u) = ObjectType (o <> [i]) i p n v u

makeOidWithFirstAsBase :: [MIB] -> [MIB]
makeOidWithFirstAsBase [] = []
makeOidWithFirstAsBase (y:ys) = makePartedOid [(parent y, init $ oid y)] (y:ys)
    where
        makePartedOid :: [(Name, OID)] -> [MIB] -> [MIB]
        makePartedOid _ [] = []
        makePartedOid base (x:xs) =
            let Just prev = lookup (parent x) base
                newbase = (name x, prev <> [int x]) : base
            in addOid prev x : makePartedOid newbase xs

fromListWithFirstAsBase :: [MIB] -> MIBTree 
fromListWithFirstAsBase ys = 
    let full = foldl1 insert . map singleton . sort . makeOidWithFirstAsBase $ ys
    in dropParted full
    where
    dropParted y@(Node o _ _ _ _ z@(Node o1 _ _ _ _ _ _) _) 
      | o == o1 = dropParted z
      | otherwise = y
    dropParted y@_ = y

toList :: MIBTree -> [MIB]
toList Empty = []
toList (Leaf o i p n next v u) = ObjectType o i p n v u  : toList next
toList (Node o i p n next link u) = Object o i p n u : toList next <> toList link

printTree :: MIBTree -> IO ()
printTree f = putStr $ unlines $ drawLevel f
  where
    drawLevel Empty = []
    drawLevel (Node o i _ n next link _) = ("Object " <> show i <> " " <> n <> " " <> show o ) : (drawSubtree next link)
    drawLevel (Leaf o i _ n next v _) = ("ObjectType " <> show i <> " " <> n <> " " <> show v <> " " <> show o) : (drawSubtree next Empty)
    
    drawSubtree next link = (shift "`- " " | " (drawLevel link)) <> drawLevel next
      where 
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
goNext (Node _ _ _ _ Empty _ _, _) = Nothing
goNext (Leaf _ _ _ _ Empty _ _, _) = Nothing
goNext (Leaf o i p n next v    u, bs) = Just (next, Next (Leaf o i p n Empty v u):bs)
goNext (Node o i p n next link u, bs) = Just (next, Next (Node o i p n Empty link u):bs)

goLevel :: Zipper -> Maybe Zipper 
goLevel (Empty, _) = Nothing
goLevel (Leaf{}, _) = Nothing
goLevel (Node _ _ _ _ _ Empty _, _) = Nothing
goLevel (Node o i p n next link u, bs) = Just (link, Level (Node o i p n next Empty u):bs)

goBack :: Zipper -> Maybe Zipper 
goBack (_, []) = Nothing
goBack (t, Next (Leaf o i p n Empty v u):bs) = Just (Leaf o i p n t v u, bs)
goBack (t, Next (Node o i p n Empty link u):bs) = Just (Node o i p n t link u, bs)
goBack (t, Level (Node o i p n next Empty u):bs) = Just (Node o i p n next t u, bs)
goBack _ = Nothing

goUp :: Zipper -> Maybe Zipper 
goUp (_, []) = Nothing
goUp (t, Next (Leaf o i p n Empty v u):bs) = goUp (Leaf o i p n t v u, bs)
goUp (t, Next (Node o i p n Empty link u):bs) = goUp (Node o i p n t link u, bs)
goUp (t, Level (Node o i p n next Empty u):bs) = Just (Node o i p n next t u, bs)
goUp _ = Nothing

top :: Zipper -> Zipper
top (t,[]) = (t,[])  
top z = top (fromJust $ goBack z)

getFocus :: Zipper -> MIB
getFocus = fromTree . fst 
  where
  fromTree Empty = undefined
  fromTree (Node o i p n _ _ u) = Object o i p n u
  fromTree (Leaf o i p n _ v u) = ObjectType o i p n v u

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
           modify $ attach $ fromListWithFirstAsBase (c:n)
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
    o <- oid . getFocus <$> get
    walk' $ fromMaybe [] (stripPrefix (init o) xs)
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
    o <- oid . getFocus <$> get
    t <- isObjectType
    case (o == xs, t) of
         (True, True) -> update Nothing=<< getFocus <$> get
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

focus :: Base ()
focus = liftIO . print =<< getFocus <$> get

findNext :: SearchRange -> Base MIB
findNext s@(SearchRange (start, end, True)) = do
    goClosest start
    o <- getFocus <$> get
    t <- isObjectType
    if oid o == start && t
       then return $ inRange s o
       else inRange s <$> findNext (SearchRange (start, end, False))
findNext s@(SearchRange (start, _end, False)) = do
    goClosest start
    l <- hasLevel
    n <- hasNext
    case (l, n) of
         (True, _) -> do
             liftIO $ print "level"
             modify $ fromJust . goLevel
             m <- (findClosestObject' False start)
             return $ inRange s m
         (False, True) -> do
             liftIO $ print "next"
             modify $ fromJust . goNext
             m <- (findClosestObject' False start)
             return $ inRange s m
         (False, False) -> do
             liftIO $ print "up"
             modify $ fromJust . goUp
             m <- (findClosestObject' True start)
             return $ inRange s m

inRange :: SearchRange -> MIB -> MIB
inRange (SearchRange (from, to, _)) m = 
  if from <= oid m && oid m < to 
     then m 
     else (ObjectType from 0 "" "" EndOfMibView Fixed)

findClosestObject' :: Bool -> OID -> Base MIB
findClosestObject' back oid' = do
    focus
    t <- isObjectType
    l <- (if back then not else id) <$> hasLevel
    n <- hasNext
    case (t, l, n) of
         (True, _, _) -> getFocus <$> get
         (False, True, _) -> do
             liftIO $ print "level"
             modify (fromJust . goLevel)
             findClosestObject' False oid'
         (False, False, True) -> do
             liftIO $ print "next"
             modify (fromJust . goNext) 
             findClosestObject' False oid'
         (False, False, False) -> do 
             liftIO $ print "up"
             st <- get
             case (goUp st) of
                  Just ust -> put ust >> findClosestObject' True oid'
                  Nothing -> return $ ObjectType oid' 0 "" "" EndOfMibView Fixed

hasLevel :: StateT Zipper IO Bool
hasLevel = isJust . goLevel <$> get

hasNext :: StateT Zipper IO Bool
hasNext = isJust . goNext <$> get


