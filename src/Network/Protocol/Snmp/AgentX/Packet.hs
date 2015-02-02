module Network.Protocol.Snmp.AgentX.Packet 
( Packet
, include
, startOID
, endOID
, SearchRange
, RError(..)
, Context
, PacketID
, SessionID
, TransactionID
, SysUptime
, PDU(..)
, VarBind(..)
, Index
, pdu
, tid
, pid
, sid
, flags
, NonRepeaters
, MaxRepeaters
, bodySizeFromHeader
, mkPacket
, defFlags
, UndoError(..)
, TestError(..)
, CommitError(..)
, TaggedError(..)
)
where

import Network.Protocol.Snmp.AgentX.Packet.Types
import Network.Protocol.Snmp.AgentX.Packet.Binary 

mkPacket :: PDU -> SessionID -> TransactionID -> PacketID -> Packet
mkPacket pdu' sid' tid' pid' = Packet 1 pdu' defFlags sid' tid' pid'

defFlags :: Flags
defFlags = Flags False False False False False
