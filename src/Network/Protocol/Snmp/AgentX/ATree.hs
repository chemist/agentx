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
            | Update (IO ATree, Maybe ([VarBind] -> IO Bool))

base :: OID -> String -> ATree
base oid name = Node (Values oid name Zero Fixed) []

leaf :: Integer -> String -> Value -> Update -> ATree 
leaf i name v fs = Node (Values [i] name v fs) []

root :: ATree -> [ATree] -> ATree
root base' leafs = base' { subForest = leafs }

instance Show Update where
    show Fixed = "fixed"
    show (Update (_, Nothing)) = "read-only"
    show (Update (_, Just _)) = "read-write"

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

findVarBind :: OID -> AStateT VarBind
findVarBind oid = do
    r <- oidV <$> getRoot
    let stripped = L.stripPrefix r oid
    liftIO $ print stripped
    modify Zip.root
    VarBind oid <$> (findA =<< maybe (throw BadPath) return stripped)

findA :: OID -> AStateT Value
findA [] = getCurrentValue
findA (x:[]) = do
    c <- Zip.firstChild <$> get
    put =<< maybe (throw DontHaveChildren) return c
    Values oi _ _ u <- label <$> get
    if x == head oi
      then update u >> getCurrentValue
      else findLastChance x
findA y@(x:xs) = do
    c <- Zip.firstChild <$> get
    put =<< maybe (throw DontHaveChildren) return c
    Values oi _ _ u <- label <$> get
    if x == head oi
       then update u >> findA xs
       else findN y

findN y@(x:xs) = do
    c <- Zip.next <$> get
    put =<< maybe (throw NotFound) return c
    Values oi _ _ u <- label <$> get
    if x == head oi
       then update u >> findA xs
       else findN y

findLastChance :: Integer -> AStateT Value
findLastChance x = do
    c <- Zip.next <$> get
    put =<< maybe (throw NotFound) return c
    Values oi _ _ u <- label <$> get
    if x == head oi
       then update u >> getCurrentValue
       else findLastChance x

update :: Update -> AStateT ()
update Fixed = return ()
update (Update (f, _)) = do
    s <- get
    n <- liftIO $ flip Zip.setTree s <$> f
    put n

getCurrentValue :: AStateT Value
getCurrentValue = do
    Values _ _ v _ <- label <$> get
    return v

oidV :: Values -> OID
oidV (Values oi _ _ _) = oi

