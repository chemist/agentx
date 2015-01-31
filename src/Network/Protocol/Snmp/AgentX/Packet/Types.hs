{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.Protocol.Snmp.AgentX.Packet.Types where

import Data.Label

import Data.Word
import Data.ByteString (ByteString)
import Data.String

import Network.Protocol.Snmp (Value(..), OID)


data Packet = Packet 
  { _version :: Version 
  , _pdu     :: PDU 
  , _flags   :: Flags 
  , _sid     :: SessionID 
  , _tid     :: TransactionID 
  , _pid     :: PacketID 
  } deriving Show

type Version = Word8

data Flags = Flags 
  { _instanceRegistration :: Bool 
  , _newIndex             :: Bool
  , _anyIndex             :: Bool
  , _nonDefaultContext    :: Bool
  , _bigEndian            :: Bool
  } deriving (Show)

newtype SessionID = SessionID Word32 deriving (Show, Eq, Enum, Bounded, Ord)

newtype TransactionID = TransactionID Word32 deriving (Show, Eq, Ord, Enum, Bounded)

newtype PacketID = PacketID Word32 deriving (Show, Eq, Ord, Enum, Bounded)

newtype Context = Context ByteString deriving (Show, Ord, Eq, IsString)

data Reason = Other
            | ParseError
            | ProtocolError
            | Timeouts
            | Shutdown
            | ByManager
            deriving (Show, Eq)

data SearchRange = SearchRange 
  { _startOID :: OID
  , _endOID   :: OID
  , _include  :: Bool
  } deriving (Show, Eq)

data VarBind = VarBind 
  { _vboid   :: OID 
  , _vbvalue :: Value 
  } deriving (Show, Eq)

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
            -- 7.2.4.1
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
            -- 7.2.4.2
            | CommitFailed
            -- 7.2.4.3
            | UndoFailed
            deriving (Show, Eq)

data PDU = Open 
  { _timeout      :: Word8
  , _oid          :: OID 
  , _description  :: ByteString -- 6.2.1
  }      | Close 
  { _reason       :: Reason                 -- 6.2.2
  }      | Register   
  { _context      :: Maybe Context 
  , _timeout      :: Word8
  , _priority     :: Word8
  , _rangeSubid   :: Word8  
  , _subtree      :: OID 
  , _upperBound   :: Maybe Word32 -- 6.2.3
  }      | Unregister 
  { _context      :: Maybe Context        
  , _priority     :: Word8 
  , _rangeSubid   :: Word8
  , _subtree      :: OID 
  , _upperBound   :: Maybe Word32 -- 6.2.4
  }      | Get        
  { _context      :: Maybe Context 
  , _oids         :: [OID] -- 6.2.5
  }      | GetNext    
  { _context      :: Maybe Context 
  , _searchRanges :: [SearchRange] -- 6.2.6
  }      | GetBulk    
  { _context      :: Maybe Context 
  , _nonRepeaters :: Word16 
  , _maxRepeaters :: Word16 
  , _searchRanges :: [SearchRange] -- 6.2.7
  }      | TestSet    
  { _context      :: Maybe Context 
  , _varBinds     :: [VarBind]  -- 6.2.8
  }      | CommitSet -- 6.2.9
         | UndoSet -- 6.2.9
         | CleanupSet -- 6.2.9
         | Notify 
  { _context      :: Maybe Context 
  , _varBinds     :: [VarBind] -- 6.2.10
  }      | Ping       
  { _context      :: Maybe Context  -- 6.2.11
  }      | IndexAllocate   
  { _context      :: Maybe Context 
  , _varBinds     :: [VarBind] -- 6.2.12
  }      | IndexDeallocate  -- 6.2.13
  { _context      :: Maybe Context 
  , _varBinds     :: [VarBind] -- 6.2.12
  }      | AddAgentCaps    
  { _context      :: Maybe Context 
  , _oid          :: OID 
  , _description  :: ByteString  -- 6.2.14
  }      | RemoveAgentCaps 
  { _context      :: Maybe Context 
  , _oid          :: OID  -- 6.2.15
  }      | Response   
  { _sysUptime    :: Word32 
  , _rerror       :: RError 
  , _index        :: Word16 
  , _varBinds     :: [VarBind] -- 6.2.16
  } deriving (Show, Eq)

mkLabels [''PDU, ''Packet, ''Flags, ''VarBind, ''SearchRange]

class Tag a b where
    tag :: a -> b
    unTag :: b -> a

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
    tag CommitFailed          = 14
    tag UndoFailed            = 15

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
    unTag 14  = CommitFailed    
    unTag 15  = UndoFailed    
    unTag _   = error "bad rerror"        

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




