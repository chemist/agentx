{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.Protocol.Snmp.AgentX.MIBTree 
( MIB
, Base
, mkObject
, mkModule
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
, find
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
, change
, getFocus
, focus
, top
, fromListWithBase
, fromListWithFirst
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
import Data.List (stripPrefix)
import Control.Monad.State (StateT, get, liftIO, modify)
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception, throw)

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Protocol (SearchRange(..))

data MIB = Module OID Integer Parent Name
         | Object OID Integer Parent Name (Maybe UTree)
         | ObjectType OID Integer Parent Name Value Update
         | TempMib Integer
         deriving (Eq)

instance Ord MIB where
    compare x y = compare (oid x) (oid y)

instance Show MIB where
    show (Module o i p n) = "Module " <> oidToString o <> " " <> show i <> " " <> show p <> " " <> show n
    show (Object o i p n u) = "Object " <> oidToString o <> " " <> show i <> " " <> show p <> " " <> show n <> " " <> show u
    show (ObjectType o i p n v u) = "ObjectType " <> oidToString o <> " " <> show i <> " " <> show p <> " " <> show n <> " " <> show v <> " " <> show u
    show (TempMib x) = "TempMib " <> show x

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
    parent (Module _ _ x _) = x
    parent (Object _ _ x _ _) = x
    parent (ObjectType _ _ x _ _ _) = x
    parent _ = throw BadTempMib
    name (Module _ _ _ x) = x
    name (Object _ _ _ x _) = x
    name (ObjectType _ _ _ x _ _) = x
    name _ = throw BadTempMib
    int (Module _ x _ _) = x
    int (Object _ x _ _ _) = x
    int (ObjectType _ x _ _ _ _) = x
    int (TempMib x) = x
    oid (Module x _ _ _) = x
    oid (Object x _ _ _ _) = x
    oid (ObjectType x _ _ _ _ _) = x
    oid _ = throw BadTempMib

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
                

data MIBTree a = 
    Fork { object :: a
         , next :: MIBTree a
         , link :: MIBTree a
         }          
    | Empty
    deriving (Show, Eq)

-- iso(1) org(3) dod(6) internet(1) mgmt(2) mib-2(1)
iso :: MIB
iso = Module [1] 1 "" "iso"

org :: MIB
org = Module [1,3] 3 "iso" "org"

dod :: MIB
dod = Module [1,3,6] 6 "org" "dod"

internet :: MIB 
internet = Module [1,3,6,1] 1 "dod" "internet"

private :: MIB  
private = Module [1,3,6,1,4] 4 "internet" "private"

enterprise :: MIB 
enterprise = Module [1,3,6,1,4,1] 1 "private" "enterprise"

data MIBException = BadTempMib
                  | CantReadValue
                  | NotDescribedUpdate
                  | HasSuchObject 
                  deriving (Show, Typeable)

instance Exception MIBException

instance Functor MIBTree where
    fmap _ Empty = Empty
    fmap f (Fork o n l) = Fork (f o) (fmap f n) (fmap f l) 

singleton :: MIB -> MIBTree MIB
singleton m = singleton' (oid m, m) 
    where
    singleton' ([],_) = Empty
    singleton' ([_], m') = Fork m' Empty Empty
    singleton' ((x:xs), m') = Fork (TempMib x) Empty $ singleton' (xs, m')

insert :: MIBTree MIB -> MIBTree MIB -> MIBTree MIB
insert x Empty = x
insert Empty x = x
insert (Fork x nx lx) yt@(Fork y ny ly) 
    | int x == int y = Fork (x `splitMIB` y) (nx `insert` ny) (lx `insert` ly)
    | otherwise = Fork x (nx `insert` yt) lx
    where
        splitMIB :: MIB -> MIB -> MIB
        splitMIB (TempMib a) (TempMib b) | a == b = TempMib a
                                         | otherwise = throw HasSuchObject
        splitMIB (TempMib a) b | int b == a = b
                               | otherwise = throw HasSuchObject
        splitMIB b (TempMib a) | int b == a = b
                               | otherwise = throw HasSuchObject
        splitMIB a b | a == b = a
                     | otherwise = throw HasSuchObject

fromList :: [MIB] -> MIBTree MIB
fromList = foldl1 insert . map singleton . makeOid

fromListWithFirst :: MIB -> [MIB] -> MIBTree MIB
fromListWithFirst x xs = fromListWithBase (parent x) (init $ oid x) xs

fromListWithBase :: Name -> OID -> [MIB] -> MIBTree MIB
fromListWithBase n m xs = 
    let full = foldl1 insert . map singleton . makePartedOid [(n,m)] $ xs
    in dropParted full
    where
    dropParted Empty = Empty
    dropParted x@(Fork m' _ l) = if isTempMib m'
                                     then dropParted l
                                     else x
    isTempMib TempMib{} = True
    isTempMib _ = False

toList :: MIBTree MIB -> [MIB]
toList Empty = []
toList (Fork o n l) = o : toList n <> toList l

find :: OID -> MIBTree MIB -> Maybe MIB
find [] _ = Nothing
find _ Empty = Nothing
find [x] bt 
  | int (object bt) == x = Just $ object bt
  | int (object bt) /= x = find [x] (next bt)
find (x:xs) bt
  | int (object bt) == x = find xs (link bt)
  | otherwise = find (x:xs) (next bt)

printTree :: MIBTree MIB -> IO ()
printTree f = putStr $ unlines $ drawLevel f
  where
    drawLevel Empty = []
    drawLevel (Fork o n l) = (show o) : (drawSubtree n l)
    
    drawSubtree n l = (shift "`- " " | " (drawLevel l)) <> drawLevel n
      where 
      shift first rest = zipWith (++) (first : repeat rest)

makeOid :: [MIB] -> [MIB]
makeOid xs = makeOid' [] xs
  where
    makeOid' :: [(Name, OID)] -> [MIB] -> [MIB]
    makeOid' _ [] = []
    makeOid' _ (x@(Module [1] 1 "" "iso"):ys) = x : makeOid' [("iso", [1])] ys
    makeOid' base (x:ys) =
       let Just prev = lookup (parent x) base 
           newbase = (name x, prev <> [int x]) : base
       in addOid prev x : makeOid' newbase ys

addOid :: OID -> MIB -> MIB
addOid o (Module _ i p n ) = Module (o <> [i]) i p n
addOid o (Object _ i p n u) = Object (o <> [i]) i p n u
addOid o (ObjectType _ i p n v u) = ObjectType (o <> [i]) i p n v u
addOid _ _ = throw BadTempMib

makePartedOid :: [(Name, OID)] -> [MIB] -> [MIB]
makePartedOid _ [] = []
makePartedOid base (x:xs) =
    let Just prev = lookup (parent x) base
        newbase = (name x, prev <> [int x]) : base
    in addOid prev x : makePartedOid newbase xs

mkModule :: Integer -> Parent -> Name -> MIB
mkModule = Module [] 

mkObject :: Integer -> Parent -> Name -> Maybe UTree -> MIB
mkObject = Object []

mkObjectType :: Integer -> Parent -> Name -> Value -> Update -> MIB
mkObjectType = ObjectType []


---------------------------------------------------------------------------------------------------------
-- zipper
---------------------------------------------------------------------------------------------------------

data Move a = Next a (MIBTree a) | Level a (MIBTree a) deriving (Show)

type Moving a = [Move a]

type Zipper a = (MIBTree a, Moving a)

change :: (a -> a) -> Zipper a -> Zipper a
change _ (Empty, bs) = (Empty, bs)
change f (Fork a n l, bs) = (Fork (f a) n l, bs)

attach :: MIBTree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

goNext :: Zipper a -> Maybe (Zipper a)
goNext (Empty, _) = Nothing
goNext (Fork _ Empty _, _) = Nothing
goNext (Fork a n l, bs) = Just (n, Next a l:bs)

goLevel :: Zipper a -> Maybe (Zipper a)
goLevel (Empty, _) = Nothing
goLevel (Fork _ _ Empty, _) = Nothing
goLevel (Fork a n l, bs) = Just (l, Level a n:bs)

goUp :: Zipper a -> Maybe (Zipper a)
goUp (_, []) = Nothing
goUp (t, Next a l:bs) = Just (Fork a t l, bs)
goUp (t, Level a n:bs) = Just (Fork a n t, bs)

toZipper :: MIBTree a -> Zipper a
toZipper t = (t, [])

getFocus :: Zipper a -> a
getFocus = fromTree . fst 
  where
  fromTree Empty = undefined
  fromTree (Fork a _ _) = a

top :: Zipper a -> Zipper a
top (t,[]) = (t,[])  
top z = top (fromJust $ goUp z)

type Base = StateT (Zipper MIB) IO

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
           modify $ attach $ fromListWithFirst c n
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
    t <- isObjectType . getFocus <$> get
    case (o == xs, t) of
         (True, True) -> update Nothing=<< getFocus <$> get
         (True, False) -> return $ ObjectType o (last o) "" "" NoSuchObject Fixed
         _ -> return $ ObjectType o (last o) "" "" NoSuchInstance Fixed

findMany :: [OID] -> Base [MIB]
findMany [] = return []
findMany (x:xs) = (:) <$> findOne x <*> findMany xs

isObjectType :: MIB -> Bool
isObjectType ObjectType{} = True
isObjectType _            = False

focus :: Base ()
focus = liftIO . print =<< getFocus <$> get

findNext :: SearchRange -> Base MIB
findNext (SearchRange (_start, _end)) = undefined

hasLevel :: StateT (Zipper MIB) IO Bool
hasLevel = isJust . goLevel <$> get

hasNext :: StateT (Zipper MIB) IO Bool
hasNext = isJust . goNext <$> get



