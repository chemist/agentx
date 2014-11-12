module Network.Protocol.Snmp.AgentX where

import Data.Word
import Data.Binary
import Data.Binary.Put (putBuilder)
import Data.Binary.Builder
import qualified Network.Protocol.Snmp as Snmp
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Applicative hiding (empty)
import Data.Monoid

data Packet = Packet Version PDU Flags SessionID TransactionID PacketID PayloadLenght deriving Show

data PDU = Open 
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

----------------------------------------------------------------------------------------------------------------------
-- VarBind 
----------------------------------------------------------------------------------------------------------------------

data VarBind = VarBind OID Value deriving (Show, Eq)

newtype OID = OID Snmp.OID deriving (Show, Eq)

newtype Value = Value Snmp.Value deriving (Show, Eq)

varBindToBuilder :: NetworkByteOrder -> VarBind -> Builder
varBindToBuilder bo (VarBind o v) = 
     builder16 bo (getType v) <> builder16 bo 0 
  <> oidToBuilder bo o 
  <> valueToBuilder bo v


builder64 True = putWord64be
builder64 False = putWord64le
builder32 True = putWord32be
builder32 False = putWord32le
builder16 True = putWord16be
builder16 False = putWord16le

oidToBuilder :: Bool -> OID -> Builder
oidToBuilder bo (OID (1:3:6:1:xs)) = 
  singleton (fromIntegral (length xs - 1)) <> singleton (fromIntegral (head xs)) <> singleton 0 <> singleton 0
  <> (foldl1 (<>) (map (singleton . fromIntegral) (tail xs)))
oidToBuilder bo (OID xs) = 
  singleton (fromIntegral (length xs )) <> singleton 0 <> singleton 0 <> singleton 0
  <> (foldl1 (<>) (map (singleton . fromIntegral) xs))

bsToBuilder :: Bool -> ByteString -> Builder
bsToBuilder bo bs = builder32 bo (fromIntegral (B.length bs)) <> fromByteString bs <> tailB 
  where
  tailB = fromByteString $ B.replicate (fromIntegral tailLen) 0x00
  tailLen :: Int
  tailLen = fromIntegral (4 - B.length bs `rem` 4) `rem` 4

valueToBuilder :: Bool -> Value -> Builder
valueToBuilder bo (Value (Snmp.Integer x)) = builder32 bo $ fromIntegral x
valueToBuilder bo (Value (Snmp.Counter32 x)) = builder32 bo $ fromIntegral x
valueToBuilder bo (Value (Snmp.Counter64 x)) = builder64 bo $ fromIntegral x
valueToBuilder bo (Value (Snmp.Gaude32 x)) = builder32 bo $ fromIntegral x
valueToBuilder bo (Value (Snmp.TimeTicks x)) = builder32 bo $ fromIntegral x
valueToBuilder bo (Value (Snmp.OI xs)) = oidToBuilder bo (OID xs)
valueToBuilder bo (Value (Snmp.String xs)) = bsToBuilder bo xs
valueToBuilder bo (Value (Snmp.Opaque xs)) = bsToBuilder bo xs
valueToBuilder bo (Value (Snmp.IpAddress a b c d)) = bsToBuilder bo $ B.pack [a,b,c,d]
valueToBuilder bo (Value (Snmp.Zero)) = empty
valueToBuilder bo (Value (Snmp.NoSuchObject)) = empty
valueToBuilder bo (Value (Snmp.NoSuchInstance)) = empty
valueToBuilder bo (Value (Snmp.EndOfMibView)) = empty


getType :: Value -> Word16
getType (Value x) = getType' x
  where
  getType' (Snmp.Integer _) = 2
  getType' (Snmp.String _) = 4
  getType' (Snmp.Zero) = 5
  getType' (Snmp.OI _) = 6
  getType' (Snmp.IpAddress _ _ _ _) = 64
  getType' (Snmp.Counter32 _) = 65
  getType' (Snmp.Gaude32 _) = 66
  getType' (Snmp.TimeTicks _) = 67
  getType' (Snmp.Opaque _) = 68
  getType' (Snmp.Counter64 _) = 70
  getType' (Snmp.NoSuchObject) = 128
  getType' (Snmp.NoSuchInstance) = 129
  getType' (Snmp.EndOfMibView) = 130


