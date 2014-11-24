{-# LANGUAGE DeriveDataTypeable #-}
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

data Base = Base OID String Values

data Values = Values OID String Value Update deriving (Show)

type ATree = Tree Values

data Update = Fixed
            | ReadOnly (IO ATree)
            | WriteOnly (ATree -> IO ())
            | ReadWrite (IO ATree) (ATree -> IO ())

base :: OID -> String -> ATree
base oid name = Node (Values oid name Zero Fixed) []

leaf :: Integer -> String -> Value -> Update -> ATree 
leaf i name v fs = Node (Values [i] name v fs) []

root :: ATree -> [ATree] -> ATree
root base' leafs = base' { subForest = leafs }

instance Show Update where
    show Fixed = "fixed"
    show (ReadOnly _) = "read-only"
    show (WriteOnly _) = "write-only"
    show (ReadWrite _ _) = "read-write"

instance Show Base where
    show (Base oi n v) = 
        oidToString oi <> " " <> n <>  "\n"
        <> "  " <> show v

oidToString :: OID -> String
oidToString xs = init $ foldr (\a b -> show a <> "." <> b) "" xs

type AStateT = StateT (TreePos Full Values) IO

view :: AStateT ()
view = do
    s <- get
    liftIO $ putStr $ drawTree $ fmap show $ toTree s

getRoot :: AStateT Values
getRoot = Zip.label . Zip.root <$> get

data FindE = BadPath
           | DontHaveChildren
           | NotFound
           deriving (Show, Typeable)

instance Exception FindE

find :: OID -> AStateT (OID, Values)
find oid = do
    r <- oidV <$> getRoot
    let stripped = L.stripPrefix r oid
    liftIO $ print stripped
    modify Zip.root
    (,) <$> return oid <*> (findA =<< maybe (throw BadPath) return stripped)
    where
      findA :: OID -> AStateT Values
      findA [] = getCurrentValue
      findA (x:[]) = do
          c <- Zip.firstChild <$> get
          put =<< maybe (throw DontHaveChildren) return c
          Values oi _ _ u <- label <$> get
          if x == head oi
            then reread u >> getCurrentValue
            else findL x
      findA y@(x:xs) = do
          c <- Zip.firstChild <$> get
          put =<< maybe (throw DontHaveChildren) return c
          Values oi _ _ u <- label <$> get
          if x == head oi
             then reread u >> findA xs
             else findN y
      
      findN y@(x:xs) = do
          c <- Zip.next <$> get
          put =<< maybe (throw NotFound) return c
          Values oi _ _ u <- label <$> get
          if x == head oi
             then reread u >> findA xs
             else findN y
      
      findL :: Integer -> AStateT Values
      findL x = do
          c <- Zip.next <$> get
          put =<< maybe (throw NotFound) return c
          Values oi _ _ u <- label <$> get
          if x == head oi
             then reread u >> getCurrentValue
             else findL x

reread :: Update -> AStateT ()
reread Fixed = return ()
reread (ReadOnly f) = do
    s <- get
    n <- liftIO $ flip Zip.setTree s <$> f
    put n
reread (ReadWrite f _) = do
    s <- get
    n <- liftIO $ flip Zip.setTree s <$> f
    put n
reread (WriteOnly _) = return ()

getCurrentValue :: AStateT Values
getCurrentValue =  label <$> get

oidV :: Values -> OID
oidV (Values oi _ _ _) = oi

