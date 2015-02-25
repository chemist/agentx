module Network.Protocol.Snmp.AgentX.Types where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Network.Socket hiding (recv)
import Control.Concurrent.MVar
import Control.Applicative
import Data.Tuple (swap)
import Data.Map

import Network.Protocol.Snmp.AgentX.Packet 
import Network.Protocol.Snmp.AgentX.MIBTree
import Network.Protocol.Snmp


data Transaction = Transaction
  { tcontext :: Maybe Context
  , vblist :: [Value]
  , statusV :: TransactionState
  } deriving Show

data TransactionState = TestSetT
                      | CommitSetT
                      | UndoSetT
                      | CleanupSetT
                      deriving (Show, Eq, Ord, Enum)

data ST = ST
  { sysuptime :: MVar SysUptime
  , packetCounter :: MVar PacketID
  , mibs :: MVar (Module IO (PVal IO))
  , sock :: Socket
  , sessions :: MVar SessionID
  , transactions :: MVar (Map TransactionID Transaction)
  }

type AgentT = ReaderT ST IO


bridgeToBase :: MIBTree IO (PVal IO) a -> AgentT a
bridgeToBase f = do
    st <- mibs <$> ask
    lift $ modifyMVar st $ \x -> swap <$> runStateT f x

