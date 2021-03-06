{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.Protocol.Snmp.AgentX.Packet.Types 
( PDU(..)
, Version
, SessionID
, TransactionID
, PacketID
, econvert
, Context(..)
, Tag(..)
, TaggedError(..)
, TestError(..)
, CommitError(..)
, RError(..)
, UndoError(..)
, SysUptime
, Index
, MaxRepeaters
, NonRepeaters
-- ** Packet
, Packet
-- *** constructor
, mkPacket
-- *** lenses
, flags
, version 
, pdu
, sid
, pid
, tid
-- ** SearchRange 
, SearchRange
-- *** constructor
, mkSearchRange
-- *** lenses
, startOID
, endOID
, include
-- ** VarBind containt Value and OID
, VarBind
-- *** constructor
, mkVarBind
-- *** lenses
, vboid
, vbvalue
-- ** Flags 
, Flags
-- *** constructor
, mkFlags
, InstanceRegistration 
, NewIndex 
, AnyIndex 
, NonDefaultContext 
, BigEndian 
-- *** lenses
, instanceRegistration
, newIndex
, anyIndex
, nonDefaultContext
, bigEndian
)
where

import Data.Label

import Data.Word
import Data.ByteString (ByteString)
import Data.String
import Data.Default

import Network.Protocol.Snmp (Value(..), OID)

-- | protocol version (const 1 by default)
newtype Version = Version Word8 deriving (Show, Eq, Enum, Bounded, Ord)

-- | session id in header, rfc 2741, section 6.1
newtype SessionID = SessionID Word32 deriving (Show, Eq, Enum, Bounded, Ord)

-- | transaction id in header, rfc 2741, section 6.1
newtype TransactionID = TransactionID Word32 deriving (Show, Eq, Ord, Enum, Bounded)

-- | packet id in header, rfc 2741, section 6.1
newtype PacketID = PacketID Word32 deriving (Show, Eq, Ord, Enum, Bounded)

-- | helper for convert 
econvert :: (Enum a, Enum b) => a -> b
econvert = toEnum . fromEnum

-- | rfc 2571 section 3.3.1, rfc 2741 section 6.1.1 Context
newtype Context = Context ByteString deriving (Show, Ord, Eq, IsString)

-- | rfc 2741, section 6.2.2, Error status in agentx-close-pdu
data Reason = Other
            | ParseError
            | ProtocolError
            | Timeouts
            | Shutdown
            | ByManager
            deriving (Show, Eq)

-- | rfc 2741, 6.2.16, Error status in agentx-response-pdu
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

-- | result for testSetAIO (rfc 2741, section 7.2.4.1)
data TestError 
            = NoTestError
            | TooBig
            | NoSuchName
            | BadValue
            | ReadOnly
            | GenError
            | NoAccess
            | WrongType
            | WrongLength
            | WrongEncoding
            | WrongValue
            | NoCreation
            | InconsistentValue
            | ResourceUnavailable
            | NotWritable
            | InconsistentName
            deriving (Show, Eq)

-- | result for commitSetAIO (rfc 2741, section 7.2.4.2)
data CommitError
            = NoCommitError
            | CommitFailed
            deriving (Show, Eq) 
                  
-- | result for undoSetAIO (rfc 2741, section 7.2.4.3)
data UndoError
            = NoUndoError
            | UndoFailed
            deriving (Show, Eq)
          
type Timeout = Word8
type Priority = Word8
type RangeSubid = Word8
type Description = ByteString
type MContext = Maybe Context
type UpperBound = Maybe Word32
type SysUptime = Word32
type Index = Word16
type NonRepeaters = Word16
type MaxRepeaters = Word16

-- | Error with Tag instance
data TaggedError = forall a. (Show a, Eq a, Tag a Word16) => Tagged a 

instance Tag TaggedError Word16 where
    tag (Tagged x) = tag x
    unTag x 
      | x == 0 = Tagged NoAgentXError
      | x > 0 && x <= 18 = Tagged (unTag x :: TestError)
      | otherwise = Tagged (unTag x :: RError)

instance Eq TaggedError where
    x == y = (tag x :: Word16) == (tag y :: Word16)

instance Show TaggedError where
    show (Tagged a) = show a

    
-- | rfc 2741, section 6.2
data PDU = Open Timeout OID Description -- ^ section 6.2.1
         | Close  Reason                 -- ^ section 6.2.2
         | Register MContext Timeout Priority RangeSubid OID UpperBound -- ^ section 6.2.3
         | Unregister MContext Priority RangeSubid OID UpperBound -- ^ section 6.2.4
         | Get MContext [OID] -- ^ section 6.2.5
         | GetNext MContext [SearchRange] -- ^ section 6.2.6
         | GetBulk MContext NonRepeaters MaxRepeaters [SearchRange] -- ^ section 6.2.7
         | TestSet MContext [VarBind] -- ^ section 6.2.8
         | CommitSet -- ^ section 6.2.9
         | UndoSet -- ^ section 6.2.9
         | CleanupSet -- ^ section 6.2.9
         | Notify MContext [VarBind] -- ^ section 6.2.10
         | Ping MContext -- ^ section 6.2.11
         | IndexAllocate MContext [VarBind] -- ^ section 6.2.12
         | IndexDeallocate MContext [VarBind] -- ^ section 6.2.13
         | AddAgentCaps MContext OID Description -- ^ section 6.2.14
         | RemoveAgentCaps MContext OID  -- ^ section 6.2.15
         | Response SysUptime TaggedError Index [VarBind] -- ^ section 6.2.16
         deriving (Show, Eq)


-- | class for convert Errors to Word* and Word* to Errors
class Tag a b where
    tag :: a -> b
    unTag :: b -> a

-- | Packet type, describe agentx packet.
data Packet = Packet 
  { _version :: Version 
  , _pdu     :: PDU 
  , _flags   :: Flags 
  , _sid     :: SessionID 
  , _tid     :: TransactionID 
  , _pid     :: PacketID 
  } deriving Show

mkPacket :: Version -> PDU -> Flags -> SessionID -> TransactionID -> PacketID -> Packet
mkPacket = Packet

-- | header flags, rfc 2741, section 6.1
data Flags = Flags 
  { _instanceRegistration :: InstanceRegistration 
  , _newIndex             :: NewIndex
  , _anyIndex             :: AnyIndex
  , _nonDefaultContext    :: NonDefaultContext
  , _bigEndian            :: BigEndian
  } deriving (Show)

type InstanceRegistration = Bool
type NewIndex = Bool
type AnyIndex = Bool
type NonDefaultContext = Bool
type BigEndian = Bool

mkFlags :: InstanceRegistration -> NewIndex -> AnyIndex -> NonDefaultContext -> BigEndian -> Flags
mkFlags = Flags

-- | used for getnext and other requests (rfc 2741, section5.2 )
data SearchRange = SearchRange 
  { _startOID :: OID 
  , _endOID   :: OID
  , _include  :: Bool
  } deriving (Show, Eq)

-- | create SearchRange
mkSearchRange :: OID -> OID -> Bool -> SearchRange
mkSearchRange = SearchRange

-- | containt oid and value (rfc 2741, section 5.4)
data VarBind = VarBind 
  { _vboid   :: OID 
  , _vbvalue :: Value 
  } deriving (Show, Eq)

-- | constructor for VarBind
mkVarBind :: OID -> Value -> VarBind
mkVarBind = VarBind 

mkLabels [''Packet, ''Flags, ''VarBind, ''SearchRange ]

instance Tag Reason Word8 where
    tag Other         = 1
    tag ParseError    = 2
    tag ProtocolError = 3
    tag Timeouts      = 4
    tag Shutdown      = 5
    tag ByManager     = 6
    unTag 1 = Other        
    unTag 2 = ParseError   
    unTag 3 = ProtocolError
    unTag 4 = Timeouts     
    unTag 5 = Shutdown     
    unTag 6 = ByManager    
    unTag _ = error "unknown reasonFromTag"

instance Tag RError Word16 where
    tag NoAgentXError         = 0
    tag OpenFailed            = 256
    tag NotOpen               = 257
    tag IndexWrongType        = 258
    tag IndexAlreadyAllocated = 259
    tag IndexNonAvailable     = 260
    tag IndexNotAllocated     = 261
    tag UnsupportedContext    = 262
    tag DuplicateRegistration = 263
    tag UnknownRegistration   = 264
    tag UnknownAgentCaps      = 265
    tag RParseError           = 266
    tag RequestDenied         = 267
    tag ProcessingError       = 268
    unTag 0   = NoAgentXError        
    unTag 256 = OpenFailed           
    unTag 257 = NotOpen              
    unTag 258 = IndexWrongType       
    unTag 259 = IndexAlreadyAllocated
    unTag 260 = IndexNonAvailable    
    unTag 261 = IndexNotAllocated    
    unTag 262 = UnsupportedContext   
    unTag 263 = DuplicateRegistration
    unTag 264 = UnknownRegistration  
    unTag 265 = UnknownAgentCaps     
    unTag 266 = RParseError          
    unTag 267 = RequestDenied        
    unTag 268 = ProcessingError      
    unTag _   = error "bad rerror"        

instance Tag TestError Word16 where
    tag NoTestError           = 0
    tag TooBig                = 1
    tag NoSuchName            = 2
    tag BadValue              = 3
    tag ReadOnly              = 4
    tag GenError              = 5
    tag NoAccess              = 6
    tag WrongType             = 7
    tag WrongLength           = 8
    tag WrongEncoding         = 9
    tag WrongValue            = 10
    tag NoCreation            = 11
    tag InconsistentValue     = 12
    tag ResourceUnavailable   = 13
    tag NotWritable           = 17
    tag InconsistentName      = 18
    unTag 0   = NoTestError
    unTag 1   = TooBig                
    unTag 2   = NoSuchName            
    unTag 3   = BadValue              
    unTag 4   = ReadOnly              
    unTag 5   = GenError            
    unTag 6   = NoAccess            
    unTag 7   = WrongType           
    unTag 8   = WrongLength         
    unTag 9   = WrongEncoding       
    unTag 10  = WrongValue          
    unTag 11  = NoCreation          
    unTag 12  = InconsistentValue   
    unTag 13  = ResourceUnavailable 
    unTag 17  = NotWritable         
    unTag 18  = InconsistentName    
    unTag _   = error "unknown tag in TestError"

instance Tag CommitError Word16 where
    tag NoCommitError = 0 
    tag CommitFailed  = 14
    unTag 0 = NoCommitError
    unTag 14 = CommitFailed 
    unTag _ = error "unknown tag in CommitError"

instance Tag UndoError Word16 where
    tag NoUndoError           = 0
    tag UndoFailed            = 15
    unTag 0   = NoUndoError
    unTag 15  = UndoFailed    
    unTag _   = error "unknown tag in UndoError"



instance Tag PDU Word8 where
    tag Open{}            = 1
    tag Close{}           = 2
    tag Register{}        = 3
    tag Unregister{}      = 4
    tag Get{}             = 5
    tag GetNext{}         = 6
    tag GetBulk{}         = 7
    tag TestSet{}         = 8
    tag CommitSet{}       = 9
    tag UndoSet{}         = 10
    tag CleanupSet{}      = 11
    tag Notify{}          = 12
    tag Ping{}            = 13
    tag IndexAllocate{}   = 14
    tag IndexDeallocate{} = 15
    tag AddAgentCaps{}    = 16
    tag RemoveAgentCaps{} = 17
    tag Response{}        = 18
    unTag _ = undefined

instance Tag Value Word16 where
-- TODO check zerodotzero 
    tag (ZeroDotZero)       = 0
    tag (Integer _)         = 2
    tag (String _)          = 4
    tag (Zero)              = 5
    tag (OI _)              = 6
    tag (IpAddress _ _ _ _) = 64
    tag (Counter32 _)       = 65
    tag (Gaude32 _)         = 66
    tag (TimeTicks _)       = 67
    tag (Opaque _)          = 68
    tag (Counter64 _)       = 70
    tag (NoSuchObject)      = 128
    tag (NoSuchInstance)    = 129
    tag (EndOfMibView)      = 130
    unTag _ = undefined

instance Default Flags where
    def = mkFlags False False False False False

instance Default Version where
    def = Version 1

