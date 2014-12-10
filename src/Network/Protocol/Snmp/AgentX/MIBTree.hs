{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Protocol.Snmp.AgentX.MIBTree 
( MIB
, Base
, oid
, name
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
, MIBTree(..)
, getValue
, toZipper
, Zipper
, goNext
, goLevel
, goUp
, attach
, change
, getFocus
, top
, fromListWithBase
, fromListWithFirst
, findOID
, findR
, updateOne
, updateMulti
, MIBException(..)
)
where

import Network.Protocol.Snmp (Value(..), OID)
import Control.Exception
import Data.Monoid
import Data.Typeable
import Control.Applicative
import Data.Tree
import Data.Maybe 
import Control.Monad.State 
import qualified Data.List as L

data MIB = Module OID Integer Parent Name
         | Object OID Integer Parent Name Update
         | ObjectType OID Integer Parent Name Value 
         deriving (Show, Eq)

getValue :: MIB -> Value
getValue (ObjectType _ _ _ _ v) = v
getValue (Object _ _ _ _ _) = NoSuchObject
getValue _ = NoSuchInstance

type Parent = String
type Name = String

class Items a where
    parent :: a -> Parent
    name   :: a -> Name
    int    :: a -> Integer
    oid    :: a -> OID

instance Items MIB where
    parent (Module _ _ x _) = x
    parent (Object _ _ x _ _) = x
    parent (ObjectType _ _ x _ _) = x
    name (Module _ _ _ x) = x
    name (Object _ _ _ x _) = x
    name (ObjectType _ _ _ x _) = x
    int (Module _ x _ _) = x
    int (Object _ x _ _ _) = x
    int (ObjectType _ x _ _ _) = x
    oid (Module x _ _ _) = x
    oid (Object x _ _ _ _) = x
    oid (ObjectType x _ _ _ _) = x

data Update = Fixed
            | Read (IO [MIB])
            | ReadWrite (IO [MIB]) ([(OID, Value)] -> IO ())

instance Eq Update where
    _ == _ = True

instance Show Update where
    show Fixed = "fixed"
    show (Read _) = "read-only"
    show (ReadWrite _ _) = "read-write"


oidToString :: OID -> String
oidToString [] = ""
oidToString xs = init $ foldr (\a b -> show a <> "." <> b) "" xs

data MIBTree a = 
    Fork { object :: a
         , next :: MIBTree a
         , link :: MIBTree a
         }          
    | Link
         { node :: Integer
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

data MIBException = CantUseLinkHere
                  | NotDescribedUpdate
                  | HasSuchObject 
                  deriving (Show, Typeable)

instance Exception MIBException

instance Functor MIBTree where
    fmap f Empty = Empty
    fmap f (Fork o next link) = Fork (f o) (fmap f next) (fmap f link) 
    fmap f (Link i next link) = Link i (fmap f next) (fmap f link) 


singleton :: MIB -> MIBTree MIB
singleton m = singleton' (oid m, m) 
    where
    singleton' ([x], m) = Fork m Empty Empty
    singleton' ((x:xs), m) = Link x Empty $ singleton' (xs, m)

insert :: MIBTree MIB -> MIBTree MIB -> MIBTree MIB
insert x Empty = x
insert Empty x = x
insert yt@Link{} xt@Fork{} = insert xt yt
insert xt@(Link ix nx lx) yt@(Link iy ny ly) 
        | ix == iy = Link ix (nx `insert` ny) (lx `insert` ly)
        | otherwise = Link ix (nx `insert` yt) lx
insert xt@(Fork x nx lx) yt@(Link i ny ly)
        | int x == i = Fork x (nx `insert` ny) (lx `insert` ly)
        | otherwise = Fork x (nx `insert` yt) lx
insert xt@(Fork x nx lx) yt@(Fork y ny ly) 
        | int x == int y = Fork (x `splitMIB` y) (nx `insert` ny) (lx `insert` ly)
        | otherwise = Fork x (nx `insert` yt) lx
        where
        splitMIB :: MIB -> MIB -> MIB
        splitMIB x y | x == y = x
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
    dropParted x@Fork{} = x
    dropParted (Link _ Empty l) = dropParted l

toList :: MIBTree MIB -> [MIB]
toList Empty = []
toList (Link _ n l) = toList n <> toList l
toList (Fork o n l) = o : toList n <> toList l

find :: OID -> MIBTree MIB -> Maybe MIB
find _ Empty = Nothing
find [x] bt 
  | int (object bt) == x = Just $ object bt
  | int (object bt) /= x = find [x] (next bt)
find (x:xs) bt
  | int (object bt) == x = find xs (link bt)
  | otherwise = find (x:xs) (next bt)

printTree :: MIBTree MIB -> IO ()
printTree (Fork o n l) =
    putStr $ drawTree $ fmap show $ Node o $ convertToForest l <> convertToForest n
  where
      convertToForest :: MIBTree MIB -> Forest MIB
      convertToForest Empty = []
      convertToForest (Link _ n l) = convertToForest n <> convertToForest l
      convertToForest (Fork o Empty Empty) = [Node o []]
      convertToForest (Fork o n   l  ) = (Node o (convertToForest l)) : convertToForest n

makeOid :: [MIB] -> [MIB]
makeOid xs = makeOid' [] xs
  where
    makeOid' :: [(Name, OID)] -> [MIB] -> [MIB]
    makeOid' _ [] = []
    makeOid' _ (x@(Module [1] 1 "" "iso"):xs) = x : makeOid' [("iso", [1])] xs
    makeOid' base (x:xs) =
       let Just prev = lookup (parent x) base 
           newbase = (name x, prev <> [int x]) : base
       in addOid prev x : makeOid' newbase xs

addOid :: OID -> MIB -> MIB
addOid oid (Module _ i p n ) = Module (oid <> [i]) i p n
addOid oid (Object _ i p n u) = Object (oid <> [i]) i p n u
addOid oid (ObjectType _ i p n v) = ObjectType (oid <> [i]) i p n v

makePartedOid :: [(Name, OID)] -> [MIB] -> [MIB]
makePartedOid _ [] = []
makePartedOid base (x:xs) =
    let Just prev = lookup (parent x) base
        newbase = (name x, prev <> [int x]) : base
    in addOid prev x : makePartedOid newbase xs

mkModule :: Integer -> Parent -> Name -> MIB
mkModule = Module [] 

mkObject :: Integer -> Parent -> Name -> Update -> MIB
mkObject = Object []

mkObject' :: OID -> MIB
mkObject' oid = Object oid (last oid) "" "" Fixed

mkObjectType :: Integer -> Parent -> Name -> Value -> MIB
mkObjectType = ObjectType []

mkObjectType' :: OID -> MIB
mkObjectType' oid = ObjectType oid (last oid) "" "" NoSuchInstance 

---------------------------------------------------------------------------------------------------------
-- zipper
---------------------------------------------------------------------------------------------------------

data Move a = Next a (MIBTree a) | Level a (MIBTree a) deriving (Show)

type Moving a = [Move a]

type Zipper a = (MIBTree a, Moving a)

change :: (a -> a) -> Zipper a -> Zipper a
change f (Empty, bs) = (Empty, bs)
change f (Fork a n l, bs) = (Fork (f a) n l, bs)
change f (Link a n l, bs) = (Link a n l, bs) -- here must be replace 

attach :: MIBTree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

goNext :: Zipper a -> Maybe (Zipper a)
goNext (Fork a Empty l, bs) = Nothing
goNext (Fork a n l, bs) = Just (n, Next a l:bs)
goNext (Link a n l, bs) = throw CantUseLinkHere

goLevel :: Zipper a -> Maybe (Zipper a)
goLevel (Fork a n Empty, bs) = Nothing
goLevel (Fork a n l, bs) = Just (l, Level a n:bs)
goLevel (Link a n l, bs) = throw CantUseLinkHere

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, []) = Nothing
goUp (t, Next a l:bs) = Just (Fork a t l, bs)
goUp (t, Level a n:bs) = Just (Fork a n t, bs)

toZipper :: MIBTree a -> Zipper a
toZipper t = (t, [])

getFocus :: Zipper a -> a
getFocus = fromTree . fst 
  where
  fromTree (Fork a _ _) = a

top :: Zipper a -> Zipper a
top (t,[]) = (t,[])  
top z = top (fromJust $ goUp z)

type Base = StateT (Zipper MIB) IO

getUpdate :: MIB -> Update
getUpdate Module{} = Fixed
getUpdate (Object _ _ _ _ u) = u
getUpdate (ObjectType _ _ _ _ _) = Fixed

update :: Maybe [(OID, Value)] -> Base ()
update mv = do
    c <-  getFocus <$> get
    case getUpdate c of
         Fixed -> return ()
         Read reread -> do
             liftIO $ print "reread"
             n <- liftIO $ try reread 
             case n of
                  Right n' -> modify $ attach (fromListWithFirst c n')
                  Left (e :: SomeException) -> liftIO $ print e
         ReadWrite reread write -> do
             maybe (return ()) (liftIO . write) mv
             n <- liftIO $ try reread
             case n of
                  Right n' -> modify $ attach (fromListWithFirst c n')
                  Left (e :: SomeException) -> liftIO $ print e

findR :: OID -> Base MIB
findR = findOID Nothing

updateOne :: OID -> Value -> Base MIB
updateOne o v = findOID (Just [(o,v)]) o 

updateMulti :: [(OID, Value)] -> Base [MIB]
updateMulti (x:xs) = (:) <$> updateOne (fst x) (snd x) <*> updateMulti xs

notFound :: OID -> Base MIB
notFound oid = do
    b <- get
    case goUp b of
         Nothing -> return $ mkObject' oid
         Just nb -> do
             put nb
             c <- getFocus <$> get
             case c of
                  Object{} -> return $ mkObject' oid
                  Module{} -> return $ mkObject' oid
                  ObjectType{} -> return $ mkObjectType' oid 

findOID :: Maybe [(OID, Value)] -> OID -> Base MIB
findOID mv xs = do
    modify top
    c <- getFocus <$> get
    maybe (return $ mkObject' xs) (\x -> findOID' mv x xs) $ L.stripPrefix (L.init $ oid c) xs
    where
      findOID' :: Maybe [(OID, Value)] -> OID -> OID -> Base MIB
      findOID' mv [x] ys = do
          c <- getFocus <$> get
          if int c == x
             then update mv >> return c
             else do
                 isOk <- modifyMaybe goNext
                 if isOk
                    then findOID' mv [x] ys
                    else notFound ys
      findOID' mv (x:xs) ys = do
          c <- getFocus <$> get
          if int c == x
             then do
                 update mv -- if not last object, is save must be denied?
                 isOk <- modifyMaybe goLevel 
                 if  isOk 
                    then findOID' mv xs ys
                    else notFound ys
             else do
                 isOk <- modifyMaybe goNext
                 if isOk
                    then findOID' mv (x:xs) ys
                    else notFound ys
      modifyMaybe f = do
          st <- f <$> get
          maybe (return False) (\x -> put x >> return True) st

