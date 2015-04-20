module Network.Protocol.Snmp.AgentX.Types 
( SubAgentState(..)
, Transaction(..)
, bridgeToBase
, SubAgent
, TransactionState(..)
)
where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Network.Socket hiding (recv)
import Control.Concurrent.MVar
import Control.Applicative
import Data.Tuple (swap)
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
  { sysuptime :: MVar SysUptime
  , packetCounter :: MVar PacketID
  , mibs :: MVar Module 
  , sock :: Socket
  , sessions :: MVar SessionID
  , transactions :: MVar (Map TransactionID Transaction)
  }

type SubAgent = ReaderT SubAgentState IO


-- | run MIBTree in SubAgent context
bridgeToBase :: MIBTree IO a -> SubAgent a
bridgeToBase f = do
    st <- mibs <$> ask
    lift $ modifyMVar st $ \x -> swap <$> runStateT f x

