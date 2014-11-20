{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.Snmp.AgentX where

import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy
import Data.Binary
import Data.Monoid
import Control.Monad (forever)

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Types
import Debug.Trace

main :: IO ()
main = do
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


