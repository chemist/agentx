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
, focus
, top
, fromListWithBase
, fromListWithFirst
, findOID
, findR
, findNext
, updateOne
, updateMulti
, MIBException(..)
, walk
)
where

import Data.Typeable (Typeable)
import Data.Monoid ((<>))
import Data.Maybe (fromJust, isJust)
import Data.List (stripPrefix)
import Control.Monad.State (StateT, get, liftIO, modify, put)
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception, throw, try, SomeException)

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Protocol (SearchRange(..))

data MIB = Module OID Integer Parent Name
         | Object OID Integer Parent Name Update
         | ObjectType OID Integer Parent Name Value 
         | TempMib Integer
         deriving (Eq)

instance Ord MIB where
    compare x y = compare (oid x) (oid y)

instance Show MIB where
    show (Module o i p n) = "Module " <> oidToString o <> " " <> show i <> " " <> show p <> " " <> show n
    show (Object o i p n _) = "Object " <> oidToString o <> " " <> show i <> " " <> show p <> " " <> show n
    show (ObjectType o i p n v) = "ObjectType " <> oidToString o <> " " <> show i <> " " <> show p <> " " <> show n <> " " <> show v
    show (TempMib x) = "TempMib " <> show x

oidToString :: OID -> String
oidToString [] = ""
oidToString xs = init $ foldr (\a b -> show a <> "." <> b) "" xs

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
    parent _ = throw BadTempMib
    name (Module _ _ _ x) = x
    name (Object _ _ _ x _) = x
    name (ObjectType _ _ _ x _) = x
    name _ = throw BadTempMib
    int (Module _ x _ _) = x
    int (Object _ x _ _ _) = x
    int (ObjectType _ x _ _ _) = x
    int (TempMib x) = x
    oid (Module x _ _ _) = x
    oid (Object x _ _ _ _) = x
    oid (ObjectType x _ _ _ _) = x
    oid _ = throw BadTempMib

data Update = Fixed
            | Read (IO [MIB])
            | ReadWrite (IO [MIB]) ([(OID, Value)] -> IO ())

instance Eq Update where
    _ == _ = True

instance Show Update where
    show Fixed = "fixed"
    show (Read _) = "read-only"
    show (ReadWrite _ _) = "read-write"


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
addOid o (ObjectType _ i p n v) = ObjectType (o <> [i]) i p n v
addOid _ _ = throw BadTempMib

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
mkObject' o = Object o (last o) "" "" Fixed

mkObjectType :: Integer -> Parent -> Name -> Value -> MIB
mkObjectType = ObjectType []

mkObjectType' :: OID -> MIB
mkObjectType' o = ObjectType o (last o) "" "" NoSuchInstance 

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

getUpdate :: MIB -> Update
getUpdate Module{} = Fixed
getUpdate (Object _ _ _ _ u) = u
getUpdate (ObjectType _ _ _ _ _) = Fixed
getUpdate _ = Fixed

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
updateMulti [] = return []
updateMulti (x:xs) = (:) <$> updateOne (fst x) (snd x) <*> updateMulti xs

notFound :: OID -> Base MIB
notFound o = do
    b <- get
    case goUp b of
         Nothing -> return $ mkObject' o
         Just nb -> do
             put nb
             c <- getFocus <$> get
             case c of
                  Object{} -> return $ mkObject' o
                  Module{} -> return $ mkObject' o
                  ObjectType{} -> return $ mkObjectType' o 
                  _ -> throw BadTempMib

findOID :: Maybe [(OID, Value)] -> OID -> Base MIB
findOID mv xs = do
    modify top
    c <- getFocus <$> get
    maybe (return $ mkObject' xs) (\x -> findOID' mv x xs) $ stripPrefix (init $ oid c) xs
    where
      findOID' :: Maybe [(OID, Value)] -> OID -> OID -> Base MIB
      findOID' _ [] _ = undefined
      findOID' mvv [x] ys = do
          c <- getFocus <$> get
          if int c == x
             then update mvv >> return c
             else do
                 isOk <- modifyMaybe goNext
                 if isOk
                    then findOID' mvv [x] ys
                    else notFound ys
      findOID' mvv (x:xss) ys = do
          c <- getFocus <$> get
          if int c == x
             then do
                 update mvv -- if not last object, is save must be denied?
                 isOk <- modifyMaybe goLevel 
                 if  isOk 
                    then findOID' mvv xss ys
                    else notFound ys
             else do
                 isOk <- modifyMaybe goNext
                 if isOk
                    then findOID' mvv (x:xss) ys
                    else notFound ys
      modifyMaybe f = do
          st <- f <$> get
          maybe (return False) (\x -> put x >> return True) st

walk :: OID -> Base ()
walk xs = do
    modify top
    c <- getFocus <$> get
    maybe (return ()) (\x -> walk' (Object xs (last xs) "" "" Fixed) x) $ stripPrefix (init $ oid c) xs
    where
        walk' :: MIB -> OID -> Base ()
        walk' _ [] = return ()
        walk' m (x:ys) = do
            c <- getFocus <$> get
            update Nothing
            l <- isLevel
            n <- isNext
            let r = compare x (int c)
            case (r, l, n) of
                 (EQ, True, _) -> do
                     unless (ys == []) $ modify $ fromJust . goLevel
                     walk' m ys
                 (GT, _, True) -> do
                     modify $ fromJust . goNext
                     walk' m (x:ys)
                 _ -> return ()

focus :: Base ()
focus = liftIO . print =<< getFocus <$> get
   


findNext :: SearchRange -> Base MIB
findNext (SearchRange (start, _end)) = do
    modify top 
    c <- getFocus <$> get
    maybe (return c) (\x -> findNext' x (Object start 0 "" "" Fixed)) $ stripPrefix (init $ oid c) start
    where
      findNext' :: OID -> MIB -> Base MIB
      findNext' xs m = do
          c <- getFocus <$> get
          let r = compare m c
          l <- isLevel
          n <- isNext
          liftIO $ print $ "current: " <> show c
          liftIO $ print $ "needed : " <> show m
          liftIO $ print $ "compare " <> show r <> " isLevel " <> show l <> " isNext " <> show n
          case (r, l, n) of
               (GT, True, _) -> do
                   liftIO $ print "one"
                   modify $ fromJust . goLevel
                   findNext' (safeTail xs) m
               (GT, False, True) -> do
                   liftIO $ print "two"
                   modify $ fromJust . goUp
                   modify $ fromJust . goNext
                   findNext' (safeTail xs) m
               (LT, True, True) -> do
                   liftIO $ print "lt true true"
                   modify $ fromJust . goUp
                   modify $ fromJust . goNext
                   findNext' (oid c) c
               (LT, True, False) -> do
                   liftIO $ print "lt true false"
                   return c
               (EQ, True, _) -> do
                   liftIO $ print "three"
                   modify $ fromJust . goLevel
                   getFocus <$> get
               (EQ, False, True) -> do
                   liftIO $ print "three"
                   modify $ fromJust . goNext
                   getFocus <$> get
               _ -> error "new []"

      modifyMaybe f = do
          st <- f <$> get
          maybe (return False) (\x -> put x >> return True) st

safeTail [] = []
safeTail (_:xs) = xs
isLevel = do
    st <- get
    return $ isJust (goLevel st)
isNext = do
    st <- get
    return $ isJust (goNext st)



