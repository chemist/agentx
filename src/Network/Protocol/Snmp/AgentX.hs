module Network.Protocol.Snmp.AgentX where

import Data.Word

data Header = Header Version PDUType Flags SessionID TransactionID PacketID PayloadLenght deriving Show

data PDUType = Open
             | Close
             | Register
             | Unregister
             | Get
             | GetNext
             | GetBulk
             | TestSet
             | CommitSet
             | UndoSet
             | CleanupSet
             | Notify
             | Ping
             | IndexAllocate
             | IndexDeallocate
             | AddAgentCaps
             | RemoveAgentCaps
             | Response
             deriving (Show, Eq, Ord, Enum)

type Version = Word8

data Flags = Flags InstanceRegistration NewIndex AnyIndex NonDefaultContext NetworkByteOrder deriving (Show)

type InstanceRegistration = Bool
type NewIndex = Bool
type AnyIndex = Bool
type NonDefaultContext = Bool
type NetworkByteOrder = Bool


newtype SessionID = SessionID Word32 deriving (Show, Eq)

newtype TransactionID = TransactionID Word32 deriving (Show, Eq)

newtype PacketID = PacketID Word32 deriving (Show, Eq)

newtype PayloadLenght = PayloadLenght Word32 deriving (Show, Eq)
