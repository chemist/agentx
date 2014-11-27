{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
module Network.Protocol.Snmp.AgentX.ATree where

import Data.Tree
import Data.Tree.Zipper hiding (insert, parent)
import qualified Data.Tree.Zipper as Zip
import qualified Data.List as L
import Safe
import Control.Applicative
import Data.Monoid
import Control.Monad.State
import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Types
import Control.Exception
import Data.Typeable
import Data.Maybe

data MIB = Module OID Integer Parent Name
         | Object OID Integer Parent Name Update
         | ObjectType OID Integer Parent Name Value Update

instance Eq MIB where
    x == y = oid x == oid y

instance Ord MIB where
    compare x y = compare (oid x) (oid y)

class Items a where
    parent :: a -> Parent
    name   :: a -> Name
    int    :: a -> Integer
    oid    :: a -> OID

instance Items MIB where
    parent (Module _ _ x _) = x
    parent (Object _ _ x _ _) = x
    parent (ObjectType _ _ x _ _ _) = x
    name (Module _ _ _ x) = x
    name (Object _ _ _ x _) = x
    name (ObjectType _ _ _ x _ _) = x
    int (Module _ x _ _) = x
    int (Object _ x _ _ _) = x
    int (ObjectType _ x _ _ _ _) = x
    oid (Module x _ _ _) = x
    oid (Object x _ _ _ _) = x
    oid (ObjectType x _ _ _ _ _) = x

emptyMib = Node (Module [1,3,6,1,4444] 4444 "enterprise" "Fixmon") []

ls = [ Object [] 0 "Fixmon" "one" Fixed
     , Object [] 0 "one" "two" Fixed
     , ObjectType [] 0 "two" "six" (Integer 1) Fixed
     , Object [] 1 "one" "tree" Fixed
     , ObjectType [] 1 "two" "seven" (Integer 1) Fixed
     , ObjectType [] 1 "tree" "five" (Integer 1) Fixed
     , ObjectType [] 0 "tree" "fourt" (Integer 1) Fixed
     ]

makeOid :: [MIB] -> [MIB]
makeOid (y@(Module x i _ n):xs) = addOid [] y : makeOid' [(n, [i])] xs
makeOid (y@(Object x i _ n _):xs) = addOid [] y : makeOid' [(n, [i])] xs
makeOid (y@(ObjectType x i _ n _ _):xs) = addOid [] y : makeOid' [(n, [i])] xs

makeOid' :: [(Name, OID)] -> [MIB] -> [MIB]
makeOid' _ [] = []
makeOid' base (x:xs) =
   let Just prev = lookup (parent x) base 
       newbase = (name x, prev <> [int x]) : base
   in addOid prev x : makeOid' newbase xs

tailOid :: MIB -> MIB
tailOid (Module oi a b c) = Module (tail oi) a b c
tailOid (Object oi a b c d) = Object (tail oi) a b c d
tailOid (ObjectType oi a b c d e) = ObjectType (tail oi) a b c d e

insert :: MIB -> MIBTree -> MIBTree
insert y t 
  | oid y == [] 
     = t
  | child : childs <- subForest t, int (rootLabel child) == head (oid y)
     = t { subForest = insert (tailOid y) child : childs }
  | otherwise = t { subForest = insert (tailOid y) (Node y []) :subForest t }

mkTree :: [MIB] -> MIBTree
mkTree = sortTree . L.foldl' (flip insert) emptyMib . L.sort . makeOid

type Parent = String
type Name = String

instance Show MIB where
    show (Module oid _ _ s) = "Module " <> oidToString oid <> " " <> s 
    show (Object oid _ _ s u) = "Object " <> oidToString oid <> " " <> " " <> s <> " " <> show u
    show (ObjectType oid _ _ s v u) = "ObjectType " <> oidToString oid <> " " <> " " <> s <> " " <> show v <> " " <> show u


type MIBTree = Tree MIB
type MIBForest = [MIBTree]

data Update = Fixed
            | Read (IO MIBTree)
            | Write (MIBTree -> IO ())
            | ReadWrite (IO MIBTree) (MIBTree -> IO ())

instance Show Update where
    show Fixed = "fixed"
    show (Read _) = "read-only"
    show (Write _) = "write-only"
    show (ReadWrite _ _) = "read-write"


mModule :: OID -> Parent -> Name -> MIBForest -> MIBTree
mModule oid parent name childs = Node (Module oid (L.last oid) parent name) childs

mObject :: Integer -> Parent -> Name -> Update -> MIBForest -> MIBTree
mObject i parent name u childs = Node (Object [] i parent name u) childs

mObjectType :: Integer -> Parent -> Name -> Value -> Update -> MIBTree
mObjectType i p n v u = Node (ObjectType [] i p n v u) []

oidToString :: OID -> String
oidToString [] = ""
oidToString xs = init $ foldr (\a b -> show a <> "." <> b) "" xs

type AStateT = StateT (TreePos Full MIB) IO

view :: AStateT ()
view = do
    s <- get
    liftIO $ putStr $ drawTree $ fmap show $ toTree s

getRoot :: AStateT MIB
getRoot = Zip.label . Zip.root <$> get

data FindE = BadPath
           | DontHaveChildren
           | NotFound
           deriving (Show, Typeable)

instance Exception FindE

ufromMIB :: MIB -> Update 
ufromMIB (Object _ _ _ _ u) = u
ufromMIB (ObjectType _ _ _ _ _ u) = u

find :: OID -> AStateT (OID, MIB)
find oi = do
    r <- oid <$> getRoot
    let stripped = L.stripPrefix r oi
    liftIO $ print stripped
    modify Zip.root
    (,) <$> return oi <*> (findA =<< maybe (throw BadPath) return stripped)
    where
      findA :: OID -> AStateT MIB
      findA [] = getCurrent
      findA (x:[]) = do
          c <- Zip.firstChild <$> get
          put =<< maybe (throw DontHaveChildren) return c
          current <- getCurrent
          if x == int current
            then reread (ufromMIB current) >> getCurrent
            else findL x
      findA y@(x:xs) = do
          c <- Zip.firstChild <$> get
          put =<< maybe (throw DontHaveChildren) return c
          current <- getCurrent
          if x == int current
             then reread (ufromMIB current) >> findA xs
             else findN y
      
      findN y@(x:xs) = do
          c <- Zip.next <$> get
          put =<< maybe (throw NotFound) return c
          current <- getCurrent
          if x == int current
             then reread (ufromMIB current) >> findA xs
             else findN y
      
      findL :: Integer -> AStateT MIB
      findL x = do
          c <- Zip.next <$> get
          put =<< maybe (throw NotFound) return c
          current <- getCurrent
          if x == int current
             then reread (ufromMIB current) >> getCurrent
             else findL x

reread :: Update -> AStateT ()
reread Fixed = return ()
reread (Read f) = do
    s <- get
    n <- liftIO $ flip Zip.setTree s <$> f 
    put n
reread (ReadWrite f _) = do
    s <- get
    n <- liftIO $ flip Zip.setTree s <$> f
    put n
reread (Write _) = return ()

getCurrent :: AStateT MIB
getCurrent =  label <$> get

mibToRegisterPdu :: MIB -> PDU
mibToRegisterPdu (Module oid _ _ _ ) = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) oid Nothing
mibToRegisterPdu (Object oid _ _ _ _) = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) oid Nothing
mibToRegisterPdu (ObjectType oid _ _ _ _ _) = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) oid Nothing

addOid :: OID -> MIB -> MIB
addOid _ (Module oid t p n ) = Module oid t p n
addOid oid (Object _ i p n u) = Object (oid <> [i]) i p n u
addOid oid (ObjectType _ i p n v u) = ObjectType (oid <> [i]) i p n v u

instance Ord MIBTree where
    compare x y = compare (oid (rootLabel x)) (oid (rootLabel y))

sortTree :: MIBTree -> MIBTree
sortTree x = sortTree' . fullOidTree $ x
  where
    sortTree' x@(Node _ f) = x { subForest = L.sort (fmap sortTree' f) }

fullOidTree :: MIBTree -> MIBTree
fullOidTree x = modTree [] x
  where
    modTree :: OID -> MIBTree -> MIBTree
    modTree base (Node a []) = Node (addOid base a) []
    modTree base (Node a xs) = Node (addOid base a) (modForest (oid (addOid base a)) xs)
    modForest :: OID -> [MIBTree] -> [MIBTree]
    modForest base [] = []
    modForest base (x:xs) = modTree base x : modForest base xs

