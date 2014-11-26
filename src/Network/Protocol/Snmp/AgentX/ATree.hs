{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
module Network.Protocol.Snmp.AgentX.ATree where

import Data.Tree
import Data.Tree.Zipper
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

data MIB = Module OID String
         | Object OID Integer String Update
         | ObjectType OID Integer String Value Update

instance Show MIB where
    show (Module oid s) = "Module " <> oidToString oid <> " " <> s 
    show (Object oid _ s u) = "Object " <> oidToString oid <> " " <> " " <> s <> " " <> show u
    show (ObjectType oid _ s v u) = "ObjectType " <> oidToString oid <> " " <> " " <> s <> " " <> show v <> " " <> show u


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


mModule :: OID -> String -> MIBForest -> MIBTree
mModule oid name childs = Node (Module oid name) childs

mObject :: Integer -> String -> Update -> MIBForest -> MIBTree
mObject i name u childs = Node (Object [] i name u) childs

mObjectType :: Integer -> String -> Value -> Update -> MIBTree
mObjectType i n v u = Node (ObjectType [] i n v u) []

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

ifromMIB :: MIB -> Integer
ifromMIB (Object _ i _ _) = i
ifromMIB (ObjectType _ i _ _ _) = i

ufromMIB :: MIB -> Update 
ufromMIB (Object _ _ _ u) = u
ufromMIB (ObjectType _ _ _ _ u) = u

find :: OID -> AStateT (OID, MIB)
find oid = do
    r <- oidV <$> getRoot
    let stripped = L.stripPrefix r oid
    liftIO $ print stripped
    modify Zip.root
    (,) <$> return oid <*> (findA =<< maybe (throw BadPath) return stripped)
    where
      findA :: OID -> AStateT MIB
      findA [] = getCurrent
      findA (x:[]) = do
          c <- Zip.firstChild <$> get
          put =<< maybe (throw DontHaveChildren) return c
          current <- getCurrent
          if x == ifromMIB current
            then reread (ufromMIB current) >> getCurrent
            else findL x
      findA y@(x:xs) = do
          c <- Zip.firstChild <$> get
          put =<< maybe (throw DontHaveChildren) return c
          current <- getCurrent
          if x == ifromMIB current
             then reread (ufromMIB current) >> findA xs
             else findN y
      
      findN y@(x:xs) = do
          c <- Zip.next <$> get
          put =<< maybe (throw NotFound) return c
          current <- getCurrent
          if x == ifromMIB current
             then reread (ufromMIB current) >> findA xs
             else findN y
      
      findL :: Integer -> AStateT MIB
      findL x = do
          c <- Zip.next <$> get
          put =<< maybe (throw NotFound) return c
          current <- getCurrent
          if x == ifromMIB current
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

oidV :: MIB -> OID
oidV (Module oi _ ) = oi
oidV (Object oi i _ _) = oi
oidV (ObjectType oi i _ _ _) = oi

mibToRegisterPdu :: MIB -> PDU
mibToRegisterPdu (Module oid _ ) = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) oid Nothing
mibToRegisterPdu (Object oid _ _ _) = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) oid Nothing
mibToRegisterPdu (ObjectType oid _ _ _ _) = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) oid Nothing

addOid :: OID -> MIB -> MIB
addOid _ (Module oid t) = Module oid t
addOid oid (Object _ i s u) = Object (oid <> [i]) i s u
addOid oid (ObjectType _ i s v u) = ObjectType (oid <> [i]) i s v u

fullOidTree :: MIBTree -> MIBTree
fullOidTree x = modTree [] x
  where
    modTree :: OID -> MIBTree -> MIBTree
    modTree base (Node a []) = Node (addOid base a) []
    modTree base (Node a xs) = Node (addOid base a) (modForest (oidV (addOid base a)) xs)
    modForest :: OID -> [MIBTree] -> [MIBTree]
    modForest base [] = []
    modForest base (x:xs) = modTree base x : modForest base xs

