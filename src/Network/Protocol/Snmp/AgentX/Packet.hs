module Network.Protocol.Snmp.AgentX.Packet ( 
-- * Packet building
  Packet
-- ** create Packet
, mkPacket
-- ** create Flags
, defFlags
-- ** acess and modify Packet
, pdu
, flags
, tid
, pid
, sid
-- ** types used in Packet
, PDU(..)
, PacketID
, SessionID
, TransactionID
-- * other types
, SearchRange
, include 
, startOID 
, endOID  
, Context
, SysUptime
, VarBind
, varbind
, vbvalue
, vboid
, Index
, NonRepeaters
, MaxRepeaters
-- * types for errors 
, RError(..)
, UndoError(..)
, TestError(..)
, CommitError(..)
, TaggedError(..)
-- * other
, bodySizeFromHeader
)
where

import Network.Protocol.Snmp.AgentX.Packet.Types
import Network.Protocol.Snmp.AgentX.Packet.Binary 

-- | create Packet
mkPacket :: PDU -> SessionID -> TransactionID -> PacketID -> Packet
mkPacket pdu' sid' tid' pid' = Packet 1 pdu' defFlags sid' tid' pid'

-- | create Flags
defFlags :: Flags
defFlags = Flags False False False False False
