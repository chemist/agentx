{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Protocol.Snmp.AgentX.Packet.Binary 
( bodySizeFromHeader )
where

import Data.Binary
import Network.Protocol.Snmp.AgentX.Packet.Types ( Packet )
import Network.Protocol.Snmp.AgentX.Packet.Get ( getPacket, bodySizeFromHeader )
import Network.Protocol.Snmp.AgentX.Packet.Put ( putPacket )

instance Binary Packet where
    put = putPacket 
    get = getPacket
