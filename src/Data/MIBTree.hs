{-# LANGUAGE DeriveDataTypeable #-}
module Data.MIBTree where

import Network.Protocol.Snmp (Value(..), OID)
import Control.Exception
import Data.Monoid
import Data.Typeable
import Control.Applicative

data MIB = Module Integer Parent Name
         | Object Integer Parent Name Update
         | ObjectType Integer Parent Name Value Update
         deriving (Show, Eq)

type Parent = String
type Name = String

class Items a where
    parent :: a -> Parent
    name   :: a -> Name
    int    :: a -> Integer

instance Items MIB where
    parent (Module _ x _) = x
    parent (Object _ x _ _) = x
    parent (ObjectType _ x _ _ _) = x
    name (Module _ _ x) = x
    name (Object _ _ x _) = x
    name (ObjectType _ _ x _ _) = x
    int (Module x _ _) = x
    int (Object x _ _ _) = x
    int (ObjectType x _ _ _ _) = x

data Update = Fixed
            | Read (IO (MIBTree MIB))
            | Write (MIBTree MIB -> IO ())
            | ReadWrite (IO (MIBTree MIB)) (MIBTree MIB -> IO ())

instance Eq Update where
    _ == _ = True

instance Show Update where
    show Fixed = "fixed"
    show (Read _) = "read-only"
    show (Write _) = "write-only"
    show (ReadWrite _ _) = "read-write"


oidToString :: OID -> String
oidToString [] = ""
oidToString xs = init $ foldr (\a b -> show a <> "." <> b) "" xs

data MIBTree a = 
    Fork { object :: a
         , next :: MIBTree a
         , link :: MIBTree a
         }          
    | Empty
    deriving (Show, Eq)

-- iso(1) org(3) dod(6) internet(1) mgmt(2) mib-2(1)
iso :: MIBTree MIB -> MIBTree MIB
iso l = Fork (Module 1 "" "iso") Empty l

org :: MIBTree MIB -> MIBTree MIB
org l = iso $ Fork (Module 3 "iso" "org") Empty l

dod :: MIBTree MIB -> MIBTree  MIB
dod l = org $ Fork (Module 6 "org" "dod") Empty l

internet :: MIBTree MIB -> MIBTree MIB 
internet l = dod $ Fork (Module 1 "dod" "internet") Empty l

private :: MIBTree MIB -> MIBTree MIB  
private l = internet $ Fork (Module 4 "internet" "private") Empty l

enterprise :: MIBTree MIB -> MIBTree MIB 
enterprise l = private $ Fork (Module 1 "private" "enterprise") Empty l

fixmon :: MIBTree MIB -> MIBTree MIB 
fixmon Empty = enterprise $ Fork (Module 77777 "enterprise" "fixmon") Empty Empty
fixmon n@(Fork x _ _) | parent x == "enterprise" = enterprise $ Fork (Module 77777 "enterprise" "fixmon") n Empty
                      | parent x == "fixmon"     = enterprise $ Fork (Module 77777 "enterprise" "fixmon") Empty n

data MIBException = NotFoundParent deriving (Show, Typeable)

instance Exception MIBException

instance Functor MIBTree where
    fmap f Empty = Empty
    fmap f (Fork o next link) = Fork (f o) (fmap f next) (fmap f link) 

instance Applicative MIBTree where
    pure x = Fork x Empty Empty

