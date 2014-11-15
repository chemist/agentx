{-# LANGUAGE FlexibleInstances #-}
module Network.Protocol.Snmp.AgentX where

import Data.Word
import Data.Binary
import Data.Binary.Put (putBuilder)
import Data.Binary.Builder
import Network.Protocol.Snmp (Value(..), OID)
import qualified Network.Protocol.Snmp as Snmp
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Applicative hiding (empty)
import Data.Monoid
import Data.Bits
import Data.Bits.Bitwise (fromListLE, toListLE)

data Packet = Packet Version PDU Flags SessionID TransactionID PacketID deriving Show

newtype Timeout = Timeout Word8 deriving (Show, Eq, Ord)
newtype Description = Description ByteString deriving (Show, Eq)

data Reason = Other
            | ParseError
            | ProtocolError
            | Timeouts
            | Shutdown
            | ByManager
            deriving (Show, Eq)

instance Enum Reason where
    fromEnum Other         = 1
    fromEnum ParseError    = 2
    fromEnum ProtocolError = 3
    fromEnum Timeouts      = 4
    fromEnum Shutdown      = 5
    fromEnum ByManager     = 6
    toEnum 1 = Other        
    toEnum 2 = ParseError   
    toEnum 3 = ProtocolError
    toEnum 4 = Timeouts     
    toEnum 5 = Shutdown     
    toEnum 6 = ByManager    

newtype Context = Context ByteString deriving (Show, Eq)

newtype Priority = Priority Word8 deriving (Show, Eq)
{-
 Permits specifying a range in place of one of r.subtree's
 sub-identifiers.  If this value is 0, no range is being
 specified and there is no r.upper_bound field present in the
 PDU. In this case the MIB region being registered is the
 single subtree named by r.subtree.

 Otherwise the "r.range_subid"-th sub-identifier in r.subtree
 is a range lower bound, and the range upper bound sub-
 identifier (r.upper_bound) immediately follows r.subtree.
 In this case the MIB region being registered is the union of
 the subtrees formed by enumerating this range.
 -}
newtype RangeSubid = RangeSubid Word8 deriving (Show, Eq)
{-
 If r.subtree is in fact a fully qualified instance name, the
 INSTANCE_REGISTRATION bit in h.flags must be set, otherwise
 it must be cleared.  The master agent may save this
 information to optimize subsequent operational dispatching.
-}
type SubTree = OID
newtype UpperBound = UpperBound Word32 deriving (Show, Eq)
newtype SearchRange = SearchRange (OID, OID) deriving (Show, Eq)
newtype NonRepeaters = NonRepeaters Word16 deriving (Show, Eq)
newtype MaxRepeaters = MaxRepeaters Word16 deriving (Show, Eq)
newtype SysUptime = SysUptime Word32 deriving (Show, Eq)
newtype Index = Index Word16 deriving (Show, Eq)

data RError = NoAgentXError
            | OpenFailed
            | NotOpen
            | IndexWrongType
            | IndexAlreadyAllocated
            | IndexNonAvailable
            | IndexNotAllocated
            | UnsupportedContext
            | DuplicateRegistration
            | UnknownRegistration
            | UnknownAgentCaps
            | RParseError
            | RequestDenied
            | ProcessingError
            deriving (Show, Eq)

instance Enum RError where
    fromEnum NoAgentXError         = 0
    fromEnum OpenFailed            = 256
    fromEnum NotOpen               = 257
    fromEnum IndexWrongType        = 258
    fromEnum IndexAlreadyAllocated = 259
    fromEnum IndexNonAvailable     = 260
    fromEnum IndexNotAllocated     = 261
    fromEnum UnsupportedContext    = 262
    fromEnum DuplicateRegistration = 263
    fromEnum UnknownRegistration   = 264
    fromEnum UnknownAgentCaps      = 265
    fromEnum RParseError           = 266
    fromEnum RequestDenied         = 267
    fromEnum ProcessingError       = 268
    toEnum 0   = NoAgentXError        
    toEnum 256 = OpenFailed           
    toEnum 257 = NotOpen              
    toEnum 258 = IndexWrongType       
    toEnum 259 = IndexAlreadyAllocated
    toEnum 260 = IndexNonAvailable    
    toEnum 261 = IndexNotAllocated    
    toEnum 262 = UnsupportedContext   
    toEnum 263 = DuplicateRegistration
    toEnum 264 = UnknownRegistration  
    toEnum 265 = UnknownAgentCaps     
    toEnum 266 = RParseError          
    toEnum 267 = RequestDenied        
    toEnum 268 = ProcessingError      

data PDU = Open Timeout OID Description -- 6.2.1
         | Close Reason                 -- 6.2.2
         | Register   (Maybe Context) Timeout Priority RangeSubid SubTree (Maybe UpperBound) -- 6.2.3
         | Unregister (Maybe Context)         Priority RangeSubid SubTree (Maybe UpperBound) -- 6.2.4
         | Get        (Maybe Context) [SearchRange] -- 6.2.5
         | GetNext    (Maybe Context) [SearchRange] -- 6.2.6
         | GetBulk    (Maybe Context) NonRepeaters MaxRepeaters [SearchRange] -- 6.2.7
         | TestSet    (Maybe Context) [VarBind]  -- 6.2.8
         | CommitSet -- 6.2.9
         | UndoSet -- 6.2.9
         | CleanupSet -- 6.2.9
         | Notify     (Maybe Context) [VarBind] -- 6.2.10
         | Ping       (Maybe Context)  -- 6.2.11
         | IndexAllocate   (Maybe Context) [VarBind] -- 6.2.12
         | IndexDeallocate (Maybe Context) [VarBind] -- 6.2.13
         | AddAgentCaps    (Maybe Context) OID Description  -- 6.2.14
         | RemoveAgentCaps (Maybe Context) OID  -- 6.2.15
         | Response   SysUptime RError Index (Maybe VarBind) -- 6.2.16
         deriving (Show, Eq)

pduToTag :: PDU -> Word8
pduToTag Open{}            = 1
pduToTag Close{}           = 2
pduToTag Register{}        = 3
pduToTag Unregister{}      = 4
pduToTag Get{}             = 5
pduToTag GetNext{}         = 6
pduToTag GetBulk{}         = 7
pduToTag TestSet{}         = 8
pduToTag CommitSet{}       = 9
pduToTag UndoSet{}         = 10
pduToTag CleanupSet{}      = 11
pduToTag Notify{}          = 12
pduToTag Ping{}            = 13
pduToTag IndexAllocate{}   = 14
pduToTag IndexDeallocate{} = 15
pduToTag AddAgentCaps{}    = 16
pduToTag RemoveAgentCaps{} = 17
pduToTag Response{}        = 18

type Version = Word8

class ToBuilder a where
    toBuilder :: a 

data Flags = Flags InstanceRegistration NewIndex AnyIndex NonDefaultContext BigEndian deriving (Show)

instance ToBuilder (Flags -> Builder) where
    toBuilder (Flags instanceRegistration newIndex anyIndex nonDefCont nbo) = 
        singleton $ fromListLE [instanceRegistration, newIndex, anyIndex, nonDefCont, nbo]

flagsFromTag :: Word8 -> Flags
flagsFromTag x =
    let (i:n:a:nd:nb:_) = toListLE x
    in Flags i n a nd nb


type InstanceRegistration = Bool
type NewIndex = Bool
type AnyIndex = Bool
type NonDefaultContext = Bool

newtype SessionID = SessionID Word32 deriving (Show, Eq)

newtype TransactionID = TransactionID Word32 deriving (Show, Eq)

newtype PacketID = PacketID Word32 deriving (Show, Eq)

newtype PayloadLenght = PayloadLenght Word32 deriving (Show, Eq)

----------------------------------------------------------------------------------------------------------------------
-- VarBind 
----------------------------------------------------------------------------------------------------------------------

data VarBind = VarBind OID Value deriving (Show, Eq)

instance ToBuilder (BigEndian -> VarBind -> Builder) where
    toBuilder bo (VarBind o v) = 
     builder16 bo (getType v) <> builder16 bo 0 <> toBuilder bo False o <> toBuilder bo v


builder64 True = putWord64be
builder64 False = putWord64le
builder32 True = putWord32be
builder32 False = putWord32le
builder16 True = putWord16be
builder16 False = putWord16le

type Include = Bool
type BigEndian = Bool

instance ToBuilder (Include -> Builder) where
    toBuilder True = singleton 1
    toBuilder False = singleton 0

instance ToBuilder (BigEndian -> Include -> OID -> Builder) where
    toBuilder _ _  [] = singleton 0 <> singleton 0 <> singleton 0 <> singleton 0
    toBuilder bo i (1:3:6:1:xs) = 
      singleton (fromIntegral (length xs - 1)) <> singleton (fromIntegral (head xs)) <> toBuilder i <> singleton 0
      <> (foldl1 (<>) (map (singleton . fromIntegral) (tail xs)))
    toBuilder bo i xs = 
      singleton (fromIntegral (length xs )) <> singleton 0 <> toBuilder i <> singleton 0
      <> (foldl1 (<>) (map (singleton . fromIntegral) xs))

instance ToBuilder (BigEndian -> ByteString -> Builder) where
    toBuilder bo bs = builder32 bo (fromIntegral (B.length bs)) <> fromByteString bs <> tailB 
      where
      tailB = fromByteString $ B.replicate (fromIntegral tailLen) 0x00
      tailLen :: Int
      tailLen = fromIntegral (4 - B.length bs `rem` 4) `rem` 4

instance ToBuilder (BigEndian -> Value -> Builder) where
    toBuilder bo (Snmp.Integer x) = builder32 bo $ fromIntegral x
    toBuilder bo (Snmp.Counter32 x) = builder32 bo $ fromIntegral x
    toBuilder bo (Snmp.Counter64 x) = builder64 bo $ fromIntegral x
    toBuilder bo (Snmp.Gaude32 x) = builder32 bo $ fromIntegral x
    toBuilder bo (Snmp.TimeTicks x) = builder32 bo $ fromIntegral x
    toBuilder bo (Snmp.OI xs) = toBuilder bo False xs
    toBuilder bo (Snmp.String xs) = toBuilder bo xs
    toBuilder bo (Snmp.Opaque xs) = toBuilder bo xs
    toBuilder bo (Snmp.IpAddress a b c d) = toBuilder bo $ B.pack [a,b,c,d]
    toBuilder bo (Snmp.Zero) = empty
    toBuilder bo (Snmp.NoSuchObject) = empty
    toBuilder bo (Snmp.NoSuchInstance) = empty
    toBuilder bo (Snmp.EndOfMibView) = empty


getType :: Value -> Word16
getType (Snmp.Integer _) = 2
getType (Snmp.String _) = 4
getType (Snmp.Zero) = 5
getType (Snmp.OI _) = 6
getType (Snmp.IpAddress _ _ _ _) = 64
getType (Snmp.Counter32 _) = 65
getType (Snmp.Gaude32 _) = 66
getType (Snmp.TimeTicks _) = 67
getType (Snmp.Opaque _) = 68
getType (Snmp.Counter64 _) = 70
getType (Snmp.NoSuchObject) = 128
getType (Snmp.NoSuchInstance) = 129
getType (Snmp.EndOfMibView) = 130

----------------------------------------------------------------------------------------------------------------------
-- Packet 
----------------------------------------------------------------------------------------------------------------------

-- data Packet = Packet Version PDU Flags SessionID TransactionID PacketID deriving Show
-- data Flags = Flags InstanceRegistration NewIndex AnyIndex NonDefaultContext NetworkByteOrder deriving (Show)
instance ToBuilder (Packet -> Builder) where
    toBuilder (Packet _ p f@(Flags _ _ _ _ nbo) (SessionID sid) (TransactionID tid) (PacketID pid)) =
        let header = singleton 1 <> singleton (pduToTag p) <> toBuilder f <> singleton 0
            (body, PayloadLenght l) = pduToBuilder p f
        in header <> builder32 nbo sid <> builder32 nbo tid <> builder32 nbo pid <> builder32 nbo l <> body

pduToBuilder :: PDU -> Flags -> (Builder, PayloadLenght)
pduToBuilder = undefined
