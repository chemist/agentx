module Network.Protocol.Snmp.AgentX.Types 
( SubAgentState(..)
, Transaction(..)
, runMIBTree
, SubAgent
, TransactionState(..)
)
where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Network.Socket hiding (recv)
import Control.Applicative
import Data.IORef
import GHC.Conc
import Data.Map

import Network.Protocol.Snmp.AgentX.Packet 
import Network.Protocol.Snmp.AgentX.MIBTree


data Transaction = Transaction
  { tcontext :: Maybe Context
  , vblist :: [VarBind]
  , statusV :: TransactionState
  } deriving Show

data TransactionState = TestSetT
                      | CommitSetT
                      | UndoSetT
                      | CleanupSetT
                      deriving (Show, Eq, Ord, Enum)

data SubAgentState = SubAgentState
  { sysuptime :: IORef SysUptime
  , packetCounter :: IORef PacketID
  , mibs :: TVar Module 
  , sock :: Socket
  , sessions :: IORef (Maybe SessionID)
  , transactions :: IORef (Map TransactionID Transaction)
  }

type SubAgent = ReaderT SubAgentState IO


-- | run MIBTree in SubAgent context, with lock (access to Module read-write)
runMIBTree :: MIBTree IO a -> SubAgent a
runMIBTree f = do
    st <- mibs <$> ask
    oldst <- liftIO $ readTVarIO st
    (a, newst) <- liftIO $ runStateT f oldst
    result <- lift . atomically $ do
        if oldst == newst 
           then writeTVar st newst >> return True
           else return False
    if result
       then return a
       else runMIBTree f

