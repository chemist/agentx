module Network.Protocol.Snmp.AgentX.Types where

import Control.Monad.State
import Network.Socket hiding (recv)

import Network.Protocol.Snmp.AgentX.Protocol hiding (getValue)
import Network.Protocol.Snmp.AgentX.MIBTree



data ST = ST
  { sysuptime :: SysUptime
  , packetCounter :: PacketID
  , mibs :: Zipper 
  , sock :: Socket
  }

type AgentT = StateT ST IO


bridgeToBase :: Base a -> AgentT a
bridgeToBase f = do
    st <- get
    (result, new) <- liftIO $ runStateT f (mibs st)
    put (st { mibs = new })
    return result


