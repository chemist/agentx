module Network.Protocol.Snmp.AgentX.Types where

import Control.Monad.State
import Network.Socket hiding (recv)
import Data.IORef

import Network.Protocol.Snmp.AgentX.Protocol hiding (getValue)
import Network.Protocol.Snmp.AgentX.MIBTree



data ST = ST
  { sysuptime :: SysUptime
  , packetCounter :: IORef PacketID
  , mibs :: Zipper 
  , sock :: Socket
  , sessions :: IORef (Maybe SessionID, Maybe SessionID)
  }

type AgentT = StateT ST IO


bridgeToBase :: Base a -> AgentT a
bridgeToBase f = do
    st <- get
    (result, new) <- liftIO $ runStateT f (mibs st)
    put (st { mibs = new })
    return result


