{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Protocol.Snmp.AgentX where

import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy
import Data.Binary
import Data.Monoid
import Control.Monad (forever)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import Control.Monad.State
import Data.Time

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Types
import Debug.Trace

type Reaction m = Packet -> m Packet
type Handlers m = Reaction m -> Reaction m

data Request = SGet OID
             | SSet OID Value
             deriving (Show, Eq)

type AgentFun = IO Value

data AgentXState m = AgentXState
  { handlers :: [Handlers m]
  , sysuptime :: SysUptime
  , packetCounter :: PacketID
  , name :: ByteString
  , sock :: Socket
  }

newtype AgentX m a = AgentX { runAgentX :: StateT (AgentXState m) m a} deriving (Monad, Functor, MonadIO)

addHandler :: Handlers m -> AgentXState m -> AgentXState m 
addHandler h s = s { handlers = h : (handlers s) }

addRequest :: Request -> AgentFun -> Reaction m
addRequest (SGet oi) f (Packet _ (Get mc xs) flags sid tid pid) = 
    if elem oi xs
       then undefined
       else error "not found"

-- get :: OID -> IO Value -> AgentX IO ()
get oi f = addRequest (SGet oi) f

subagent :: ByteString -> Socket -> AgentX IO () -> IO ()
subagent name socket app =
    let st = AgentXState [] (SysUptime 0) (PacketID 1) name socket
    in (evalStateT . runAgentX) app st

work :: AgentX IO ()
work = do
    undefined
    -- addRequest (SGet [1,3,6,1,4,5])  undefined

check :: IO ()
check = do
    sock <- socket AF_UNIX Stream 0
    connect sock (SockAddrUnix "/var/agentx/master")
    let open = Open (Timeout 200) [1,3,6,1,4,1,44729,0] (Description "Haskell AgentX sub-aagent")
        register = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) [1,3,6,1,4,1,44729,0,0] Nothing 
    sendAll sock (encode $ Packet 1 open (Flags False False False False False) (SessionID 0) (TransactionID 0) (PacketID 1))
    -- for lazy wait more big chunk then i have, and here i fix size for recv
    h <- recv sock 20
    b <- recv sock (getBodySizeFromHeader h)
    let p = decode (h <> b) :: Packet
    print p
    sendAll sock (encode $ Packet 1 register (Flags False False False False False) (sessionId p) (TransactionID 1) (PacketID 2))
    h <- recv sock 20
    b <- recv sock (getBodySizeFromHeader h)
    let p = decode (h <> b) :: Packet
    print p
    do
        h <- recv sock 20
        b <- recv sock (getBodySizeFromHeader h)
        let g = decode (h <> b) :: Packet
        print g
        let response = Response (sysUptime p ) NoAgentXError (Index 0) [VarBind [1,3,6,1,4,1,44729,0,0] (Integer 100)]
        sendAll sock (encode $ Packet 1 response (Flags False False False False False) (sessionId g) (TransactionID 1) (PacketID 2))
    print "end"


