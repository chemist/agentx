{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Network.Protocol.Snmp.AgentX.ATree where

import Data.Tree
import Data.Tree.Zipper hiding (insert, parent, next)
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
import Debug.Trace

data BinTree a = 
    Fork { val :: Integer
         , object :: a
         , next :: BinTree a
         , link :: BinTree a
         }          
    | Empty
    deriving (Show, Eq)

class (Eq a, Ord a) => HasPath a where
    path :: a -> [Integer]

instance HasPath MIB where
    path (Module p _ _ _) = p
    path (Object p _ _ _ _) = p
    path (ObjectType p _ _ _ _ _) = p

instance HasPath [Integer] where
    path = id

btl :: [OID]
btl = [[0,0,1], [0,1,0], [0,1,1],[0,0,0] ]

singleton :: HasPath a => a -> BinTree a
singleton m = singleton' (path m, m) 
    where
    singleton' ([x], m) = Fork x m Empty Empty
    singleton' ((x:xs), m) = Fork x m Empty $ singleton' (xs, m)

fromList :: (Items a, HasPath a) => [a] -> BinTree a
fromList = foldl1 (<>) . map singleton 

instance Functor BinTree where
    fmap f Empty = Empty
    fmap f (Fork x m n l) = Fork x (f m) (fmap f n) (fmap f l)

toList :: HasPath a => BinTree a -> [a]
toList Empty = []
toList (Fork i o n l) = o : toList n <> toList l

data BException = TwoObjectInOnePath deriving (Typeable, Eq, Show)

instance Exception BException

instance Items a => Monoid (BinTree a) where
    mempty = Empty
    Empty `mappend` a = a
    a `mappend` Empty = a
    xt@(Fork x xo nx lx) `mappend` yt@(Fork y yo ny ly) 
        | x == y = Fork x (xo `whichLonger` yo) (nx `mappend` ny) (lx `mappend` ly)
        | otherwise = Fork x xo (yt `mappend` nx) lx
        where
        whichLonger a b = 
          let aoid = length $ oid a
              boid = length $ oid b
          in if aoid > boid
                then b
                else a

convert :: BinTree a -> Tree a
convert (Fork _ o n l) =
    Node o $ convertToForest l <> convertToForest n
  where
      convertToForest :: BinTree a -> Forest a
      convertToForest Empty = []
      convertToForest (Fork _ o Empty Empty) = [Node o []]
      convertToForest (Fork _ o n   l  ) = (Node o (convertToForest l)) : convertToForest n

find :: OID -> BinTree MIB -> Maybe MIB
find _ Empty = Nothing
find [x] bt 
  | val bt == x = Just $ object bt
  | val bt /= x = find [x] (next bt)
find (x:xs) bt
  | val bt == x = find xs (link bt)
  | otherwise = find (x:xs) (next bt)

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

-- iso(1) org(3) dod(6) internet(1) mgmt(2) mib-2(1)
enterprise = [ Module [] 1 "" "iso" 
             , Module [] 3 "iso" "org" 
             , Module [] 6 "org" "dod" 
             , Module [] 1 "dod" "internet" 
             , Module [] 4 "internet" "private" 
             , Module [] 1 "private" "enterprise" 
             ]

ls = [ Module [] 44729 "enterprise" "Fixmon"
     , Object [] 0 "Fixmon" "one" Fixed
     , Object [] 0 "one" "two" Fixed
     , ObjectType [] 0 "two" "six" (Integer 1) Fixed
     , Object [] 1 "one" "tree" Fixed
     , ObjectType [] 1 "two" "seven" (Integer 1) Fixed
     , ObjectType [] 1 "tree" "five" (Integer 1) Fixed
     , ObjectType [] 0 "tree" "fourt" (Integer 1) Fixed
     ]

makeOid :: [MIB] -> [MIB]
makeOid ((Module [] 1 "" "iso"):xs) = (Module [1] 1 "" "iso") : (makeOid' [("iso", [1])] xs)

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

type Parent = String
type Name = String

instance Show MIB where
    show (Module oid _ _ s) = "Module " <> oidToString oid <> " " <> s 
    show (Object oid _ _ s u) = "Object " <> oidToString oid <> " " <> " " <> s <> " " <> show u
    show (ObjectType oid _ _ s v u) = "ObjectType " <> oidToString oid <> " " <> " " <> s <> " " <> show v <> " " <> show u

data Update = Fixed
            | Read (IO (BinTree MIB))
            | Write (BinTree MIB -> IO ())
            | ReadWrite (IO (BinTree MIB)) (BinTree MIB -> IO ())

instance Show Update where
    show Fixed = "fixed"
    show (Read _) = "read-only"
    show (Write _) = "write-only"
    show (ReadWrite _ _) = "read-write"


oidToString :: OID -> String
oidToString [] = ""
oidToString xs = init $ foldr (\a b -> show a <> "." <> b) "" xs

ufromMIB :: MIB -> Update 
ufromMIB (Object _ _ _ _ u) = u
ufromMIB (ObjectType _ _ _ _ _ u) = u

mibToRegisterPdu :: MIB -> PDU
mibToRegisterPdu (Module oid _ _ _ ) = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) oid Nothing
mibToRegisterPdu (Object oid _ _ _ _) = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) oid Nothing
mibToRegisterPdu (ObjectType oid _ _ _ _ _) = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) oid Nothing

addOid :: OID -> MIB -> MIB
addOid oid (Module _ i p n ) = Module (oid <> [i]) i p n
addOid oid (Object _ i p n u) = Object (oid <> [i]) i p n u
addOid oid (ObjectType _ i p n v u) = ObjectType (oid <> [i]) i p n v u

instance Ord (Tree MIB) where
    compare x y = compare (oid (rootLabel x)) (oid (rootLabel y))

sortTree :: (Tree MIB) -> (Tree MIB)
sortTree x = sortTree' . fullOidTree $ x
  where
    sortTree' x@(Node _ f) = x { subForest = L.sort (fmap sortTree' f) }

fullOidTree :: (Tree MIB) -> (Tree MIB)
fullOidTree x = modTree [] x
  where
    modTree :: OID -> (Tree MIB) -> (Tree MIB)
    modTree base (Node a []) = Node (addOid base a) []
    modTree base (Node a xs) = Node (addOid base a) (modForest (oid (addOid base a)) xs)
    modForest :: OID -> [(Tree MIB)] -> [(Tree MIB)]
    modForest base [] = []
    modForest base (x:xs) = modTree base x : modForest base xs

