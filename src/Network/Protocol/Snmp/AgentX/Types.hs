module Network.Protocol.Snmp.AgentX.Types 
( SubAgentState(..)
, Transaction(..)
, evalMIBTree
, runMIBTree
, SubAgent
, TransactionState(..)
)
where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Network.Socket hiding (recv)
import Control.Concurrent.MVar
import Control.Applicative
import Data.IORef
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
  , mibs :: IORef Module 
  , mutex :: MVar ()
  , sock :: Socket
  , sessions :: IORef (Maybe SessionID)
  , transactions :: IORef (Map TransactionID Transaction)
  }

type SubAgent = ReaderT SubAgentState IO


-- | run MIBTree in SubAgent context, with lock (access to Module read-write)
runMIBTree :: MIBTree IO a -> SubAgent a
runMIBTree f = do
    st <- mibs <$> ask
    m <- mutex <$> ask
    lift $ withMVar m $ \() -> do
        (a, newst) <- runStateT f =<< (liftIO $ readIORef st)
        atomicWriteIORef st newst
        return a

-- | eval MIBTree in SubAgent context without lock (access to Module read only)
evalMIBTree :: MIBTree IO a -> SubAgent a
evalMIBTree f = do
    st <- mibs <$> ask
    liftIO $ evalStateT f =<< (liftIO $ readIORef st)
