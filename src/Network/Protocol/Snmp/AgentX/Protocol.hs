{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Network.Protocol.Snmp.AgentX.Protocol where

import Data.Word
import Data.Binary
import Data.Binary.Put (putBuilder)
import Data.Binary.Get 
import Data.Binary.Builder
import Network.Protocol.Snmp (Value(..), OID)
import qualified Network.Protocol.Snmp as Snmp
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Applicative hiding (empty)
import Data.Monoid
import Data.Bits.Bitwise (fromListLE, toListLE)
import Data.Int
import Prelude hiding (getContents)

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

reasonToTag :: Reason -> Word8
reasonToTag Other         = 1
reasonToTag ParseError    = 2
reasonToTag ProtocolError = 3
reasonToTag Timeouts      = 4
reasonToTag Shutdown      = 5
reasonToTag ByManager     = 6

reasonFromTag :: Word8 -> Reason
reasonFromTag 1 = Other        
reasonFromTag 2 = ParseError   
reasonFromTag 3 = ProtocolError
reasonFromTag 4 = Timeouts     
reasonFromTag 5 = Shutdown     
reasonFromTag 6 = ByManager    
reasonFromTag _ = error "unknown reasonFromTag"

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
newtype SearchRange = SearchRange (OID, OID, Include) deriving (Show, Eq)
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

rerrorToTag :: RError -> Word16
rerrorToTag NoAgentXError         = 0
rerrorToTag OpenFailed            = 256
rerrorToTag NotOpen               = 257
rerrorToTag IndexWrongType        = 258
rerrorToTag IndexAlreadyAllocated = 259
rerrorToTag IndexNonAvailable     = 260
rerrorToTag IndexNotAllocated     = 261
rerrorToTag UnsupportedContext    = 262
rerrorToTag DuplicateRegistration = 263
rerrorToTag UnknownRegistration   = 264
rerrorToTag UnknownAgentCaps      = 265
rerrorToTag RParseError           = 266
rerrorToTag RequestDenied         = 267
rerrorToTag ProcessingError       = 268

rerrorFromTag :: Word16 -> RError
rerrorFromTag 0   = NoAgentXError        
rerrorFromTag 256 = OpenFailed           
rerrorFromTag 257 = NotOpen              
rerrorFromTag 258 = IndexWrongType       
rerrorFromTag 259 = IndexAlreadyAllocated
rerrorFromTag 260 = IndexNonAvailable    
rerrorFromTag 261 = IndexNotAllocated    
rerrorFromTag 262 = UnsupportedContext   
rerrorFromTag 263 = DuplicateRegistration
rerrorFromTag 264 = UnknownRegistration  
rerrorFromTag 265 = UnknownAgentCaps     
rerrorFromTag 266 = RParseError          
rerrorFromTag 267 = RequestDenied        
rerrorFromTag 268 = ProcessingError      
rerrorFromTag _   = error "bad rerror"        

data PDU = Open Timeout OID Description -- 6.2.1
         | Close Reason                 -- 6.2.2
         | Register   (Maybe Context) Timeout Priority RangeSubid SubTree (Maybe UpperBound) -- 6.2.3
         | Unregister (Maybe Context)         Priority RangeSubid SubTree (Maybe UpperBound) -- 6.2.4
         | Get        (Maybe Context) [OID] -- 6.2.5
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
         | Response   SysUptime RError Index [VarBind] -- 6.2.16
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
    toBuilder (Flags instanceRegistration newIndex anyIndex ndc nbo) = 
        singleton $ fromListLE [instanceRegistration, newIndex, anyIndex, ndc, nbo]

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

instance Enum PacketID where
    toEnum = PacketID . fromIntegral
    fromEnum (PacketID x) = fromIntegral x

instance Enum SessionID where
    toEnum = SessionID . fromIntegral
    fromEnum (SessionID x) = fromIntegral x

instance Enum TransactionID where
    toEnum = TransactionID . fromIntegral
    fromEnum (TransactionID x) = fromIntegral x

newtype PayloadLenght = PayloadLenght Word32 deriving (Show, Eq)

----------------------------------------------------------------------------------------------------------------------
-- VarBind 
----------------------------------------------------------------------------------------------------------------------

data VarBind = VarBind OID Value deriving (Show, Eq)

instance ToBuilder (BigEndian -> VarBind -> Builder) where
    toBuilder bo (VarBind o v) = 
     builder16 bo (valueToTag v) <> builder16 bo 0 <> toBuilder bo False o <> toBuilder bo v


builder64 :: Bool -> Word64 -> Builder
builder64 True = putWord64be
builder64 False = putWord64le

builder32 :: Bool -> Word32 -> Builder
builder32 True = putWord32be
builder32 False = putWord32le

builder16 :: Bool -> Word16 -> Builder
builder16 True = putWord16be
builder16 False = putWord16le

get16 :: Bool -> Get Word16
get16 True = getWord16be
get16 False = getWord16le

get32 :: Bool -> Get Word32
get32 True = getWord32be
get32 False = getWord32le

get64 :: Bool -> Get Word64
get64 True = getWord64be
get64 False = getWord64le

type Include = Bool
type BigEndian = Bool

instance ToBuilder (Include -> Builder) where
    toBuilder True = singleton 1
    toBuilder False = singleton 0

instance ToBuilder (BigEndian -> Include -> OID -> Builder) where
    toBuilder _ _  [] = singleton 0 <> singleton 0 <> singleton 0 <> singleton 0
    toBuilder bo i (1:3:6:1:[]) =  -- Not clearly in rfc!!!
      singleton 4 <> singleton 0 <> toBuilder i <> singleton 0
      <> mconcat (map (builder32 bo . fromIntegral) [(1::Integer),3,6,1])
    toBuilder bo i (1:3:6:1:xs) = 
      singleton (fromIntegral (length xs - 1)) <> singleton (fromIntegral (head xs)) <> toBuilder i <> singleton 0
      <> mconcat (map (builder32 bo . fromIntegral) (tail xs))
    toBuilder bo i xs = 
      singleton (fromIntegral (length xs )) <> singleton 0 <> toBuilder i <> singleton 0
      <> mconcat (map (builder32 bo . fromIntegral) xs)

instance ToBuilder (BigEndian -> ByteString -> Builder) where
    toBuilder bo bs = builder32 bo (fromIntegral (B.length bs)) <> fromByteString bs <> tailB 
      where
      tailB = fromByteString $ B.replicate (fromIntegral tailLen) 0x00
      tailLen :: Int
      tailLen = fromIntegral (4 - B.length bs `rem` 4) `rem` 4

instance ToBuilder (BigEndian -> Value -> Builder) where
    toBuilder bo (Snmp.Integer x) = builder32 bo $ fromIntegral x
    toBuilder bo (Snmp.Counter32 x) = builder32 bo x
    toBuilder bo (Snmp.Counter64 x) = builder64 bo x
    toBuilder bo (Snmp.Gaude32 x) = builder32 bo x
    toBuilder bo (Snmp.TimeTicks x) = builder32 bo x
    toBuilder bo (Snmp.OI xs) = toBuilder bo False xs
    toBuilder bo (Snmp.String xs) = toBuilder bo xs
    toBuilder bo (Snmp.Opaque xs) = toBuilder bo xs
    toBuilder bo (Snmp.IpAddress a b c d) = toBuilder bo $ B.pack [a,b,c,d]
    toBuilder _  (Snmp.Zero) = empty
    toBuilder _  (Snmp.NoSuchObject) = empty
    toBuilder _  (Snmp.NoSuchInstance) = empty
    toBuilder _  (Snmp.EndOfMibView) = empty
    toBuilder _  (Snmp.ZeroDotZero) = empty

getValue :: BigEndian -> Word16 -> Get Value
getValue _  0 = return ZeroDotZero
getValue bo 2 = Integer . fromIntegral <$> get32 bo
getValue bo 4 = String <$> getString bo
getValue _  5 = return Zero
getValue bo 6 = OI . fst <$> getOid bo
getValue _  64 = do
    a <- getWord8
    b <- getWord8
    c <- getWord8
    d <- getWord8
    return $ IpAddress a b c d
getValue bo 65 = Counter32 <$> get32 bo
getValue bo 66 = Gaude32 <$> get32 bo
getValue bo 67 = TimeTicks <$> get32 bo
getValue bo 68 = Opaque <$> getString bo
getValue bo 70 = Counter64 <$> get64 bo
getValue _  128 = return NoSuchObject
getValue _  129 = return NoSuchInstance
getValue _  130 = return EndOfMibView
getValue _ v = error $ "getValue bad tag " ++ show v


-- TODO check zerodotzero 
valueToTag :: Value -> Word16
valueToTag (Snmp.ZeroDotZero)       = 0
valueToTag (Snmp.Integer _)         = 2
valueToTag (Snmp.String _)          = 4
valueToTag (Snmp.Zero)              = 5
valueToTag (Snmp.OI _)              = 6
valueToTag (Snmp.IpAddress _ _ _ _) = 64
valueToTag (Snmp.Counter32 _)       = 65
valueToTag (Snmp.Gaude32 _)         = 66
valueToTag (Snmp.TimeTicks _)       = 67
valueToTag (Snmp.Opaque _)          = 68
valueToTag (Snmp.Counter64 _)       = 70
valueToTag (Snmp.NoSuchObject)      = 128
valueToTag (Snmp.NoSuchInstance)    = 129
valueToTag (Snmp.EndOfMibView)      = 130

----------------------------------------------------------------------------------------------------------------------
-- Packet 
----------------------------------------------------------------------------------------------------------------------

instance ToBuilder (Packet -> Builder) where
    toBuilder (Packet _ p f@(Flags _ _ _ _ nbo) (SessionID sid) (TransactionID tid) (PacketID pid)) =
        let header = singleton 1 <> singleton (pduToTag p) <> toBuilder f <> singleton 0
            (body, PayloadLenght l) = pduToBuilder p f
        in header <> builder32 nbo sid <> builder32 nbo tid <> builder32 nbo pid <> builder32 nbo l <> body

pduToBuilder :: PDU -> Flags -> (Builder, PayloadLenght)
pduToBuilder (Open (Timeout t) o (Description d)) (Flags _ _ _ _ bi)  =
    let body = singleton t <> singleton 0 <> singleton 0 <> singleton 0
             <> toBuilder bi False o <> toBuilder bi d
    in (body, bodyLength body)
pduToBuilder (Close r) (Flags _ _ _ _ _)  =
    let body = singleton (reasonToTag r) <> singleton 0 <> singleton 0 <> singleton 0
    in (body, PayloadLenght 4)
pduToBuilder (Register mc (Timeout t) (Priority p) (RangeSubid r) s mu) (Flags _ _ _ ndc bi) =  --TODO ? INSTANCE_REGISTRATION
    let upperBound = case (r, mu) of
                          (0, _) -> empty
                          (_, Just (UpperBound u)) -> builder32 bi u
                          (_, Nothing) -> error "pduToBuilder"
        body = toBuilder bi ndc mc 
            <> singleton t <> singleton p <> singleton r <> singleton 0
            <> toBuilder bi False s
            <> upperBound
    in (body, bodyLength body)
pduToBuilder (Unregister mc (Priority p) (RangeSubid r) s mu) (Flags _ _ _ ndc bi)  =
    let upperBound = case (r, mu) of
                          (0, _) -> empty
                          (_, Just (UpperBound u)) -> builder32 bi u
                          (_, Nothing) -> error "pduToBuilder"
        body = toBuilder bi ndc mc
            <> singleton 0 <> singleton p <> singleton r <> singleton 0
            <> toBuilder bi False s
            <> upperBound
    in (body, bodyLength body)
pduToBuilder (Get mc sr) (Flags _ _ _ ndc bi)  =
    let searchRangeList = Prelude.foldr (\a b -> toBuilder bi True a <> toBuilder bi False ([] :: OID)  <> b) empty sr
        body = toBuilder bi ndc mc  <> searchRangeList
    in (body, bodyLength body)
pduToBuilder (GetNext mc sr) (Flags _ _ _ ndc bi)  =
    let searchRangeList = Prelude.foldr (\(SearchRange (s,e, _)) b -> toBuilder bi True s <> toBuilder bi False e <> b) empty sr
        body = toBuilder bi ndc mc <> searchRangeList
    in (body, bodyLength body)
pduToBuilder (GetBulk mc (NonRepeaters nr) (MaxRepeaters mr) sr) (Flags _ _ _ ndc bi)  =
    let searchRangeList = Prelude.foldr (\(SearchRange (s,e, _)) b -> toBuilder bi True s <> toBuilder bi False e <> b) empty sr
        body = toBuilder bi ndc mc
            <> builder16 bi nr <> builder16 bi mr
            <> searchRangeList
    in (body, bodyLength body)
pduToBuilder (TestSet mc vb) (Flags _ _ _ ndc bi)  =
    let vbl = Prelude.foldr (\a b -> toBuilder bi a <> b) empty vb
        body = toBuilder bi ndc mc <> vbl
    in (body, bodyLength body)
pduToBuilder CommitSet _  = (empty, PayloadLenght 0)
pduToBuilder UndoSet _  = (empty, PayloadLenght 0)
pduToBuilder CleanupSet _  = (empty, PayloadLenght 0)
pduToBuilder (Notify mc vb) (Flags _ _ _ ndc bi)  =
    let body = toBuilder bi ndc mc <> Prelude.foldr (\a b -> toBuilder bi a <> b) empty vb
    in (body, bodyLength body)
pduToBuilder (Ping mc) (Flags _ _ _ ndc bi)  =
    let body = toBuilder bi ndc mc
    in (body, bodyLength body)
pduToBuilder (IndexAllocate mc vb) (Flags _ _ _ ndc bi)  =
    let body = toBuilder bi ndc mc <> Prelude.foldr (\a b -> toBuilder bi a <> b) empty vb
    in (body, bodyLength body)
pduToBuilder (IndexDeallocate mc vb) (Flags _ _ _ ndc bi)  =
    let body = toBuilder bi ndc mc <> Prelude.foldr (\a b -> toBuilder bi a <> b) empty vb
    in (body, bodyLength body)
pduToBuilder (AddAgentCaps mc o (Description d)) (Flags _ _ _ ndc bi)  =
    let body = toBuilder bi ndc mc <> toBuilder bi False o <> toBuilder bi d
    in (body, bodyLength body)
pduToBuilder (RemoveAgentCaps mc o) (Flags _ _ _ ndc bi)  =
    let body = toBuilder bi ndc mc <> toBuilder bi False o
    in (body, bodyLength body)
pduToBuilder (Response (SysUptime su) re (Index i) vb) (Flags _ _ _ _ bi)  =
    let body = builder32 bi su
            <> builder16 bi (rerrorToTag re) <> builder16 bi i
            <> Prelude.foldr (\a b -> toBuilder bi a <> b) empty vb
    in (body, bodyLength body)

bodyLength :: Builder -> PayloadLenght
bodyLength = PayloadLenght . fromIntegral . BL.length . toLazyByteString

instance ToBuilder (BigEndian -> NonDefaultContext -> Maybe Context -> Builder) where
    toBuilder _ False _ = empty
    toBuilder bi True (Just (Context c)) = toBuilder bi c
    toBuilder _ True Nothing = error "uncorrected flag, or context"

-----------------------------------------------------------------------------------------------------------
-- binary 
-----------------------------------------------------------------------------------------------------------

instance Binary Packet where
    put = putBuilder . toBuilder 
    get = do
        version <- getWord8 
        pduTag <- getWord8
        flags <- flagsFromTag <$> getWord8
        _reserved <- getWord8
        sid <- get32 (bigEndian flags) 
        tid <- get32 (bigEndian flags) 
        pid <- get32 (bigEndian flags) 
        bodySize <- get32 (bigEndian flags) 
        pdu <- parsePdu pduTag flags bodySize
        return $ Packet version pdu flags (SessionID sid) (TransactionID tid) (PacketID pid)

type Size = Word32


getOid :: BigEndian -> Get (OID, Include)
getOid bo = do
    nSubId <- getWord8
    prefix <- getWord8
    include <- wordToInclude <$> getWord8
    _reserved <- getWord8
    end <- sequence $ replicate (fromIntegral nSubId) (get32 bo)
    case (nSubId, prefix) of
         (0, 0) -> return ([], include)
         (_, 0) -> return (map fromIntegral end, include)
         (_, x) -> return ([1,3,6,1] <> map fromIntegral (fromIntegral x:end), include)

wordToInclude :: Word8 -> Include
wordToInclude 1 = True
wordToInclude 0 = False
wordToInclude _ = error "bad include"

getOidList :: BigEndian -> [OID] -> Get [OID]
getOidList bo xs = do
    (oi, _) <- getOid bo
    isEnd <- isEmpty
    if isEnd 
       then return $ reverse (addOi oi xs)
       else getOidList bo (addOi oi xs)
    where
    addOi [] xss = xss
    addOi oi xss = oi:xss

getSearchRange :: BigEndian -> Get SearchRange
getSearchRange bo = do
    (first, include) <- getOid bo
    (second, _) <- getOid bo
    return $ SearchRange (first, second, include)

getSearchRangeList :: BigEndian -> [SearchRange] -> Get [SearchRange]
getSearchRangeList bo xs = do
    sr <- getSearchRange bo
    isEnd <- isEmpty
    if isEnd
       then return $ reverse (sr:xs)
       else getSearchRangeList bo (sr:xs)


getString :: BigEndian -> Get ByteString
getString bo = do
    l <- fromIntegral <$> get32 bo
    let fullLength = l + (4 - l `rem` 4) `rem` 4
    s <- getByteString fullLength
    return $ B.take l s
        
getDescription :: BigEndian -> Get Description
getDescription bo = Description <$> getString bo

getContext :: BigEndian -> NonDefaultContext -> Get (Maybe Context)
getContext _ False = return Nothing
getContext bo True = Just <$> Context <$> getString bo

getVarBind :: BigEndian -> Get VarBind
getVarBind bo = do
    valueTag <- get16 bo 
    _reserved <- getWord16be
    (oi, _) <- getOid bo
    v <- getValue bo valueTag 
    return $ VarBind oi v

getVarBindList :: BigEndian -> [VarBind] -> Get [VarBind]
getVarBindList bo xs = do
    vb <- getVarBind bo
    isEnd <- isEmpty
    if isEnd 
       then return $ reverse (vb:xs)
       else getVarBindList bo (vb:xs)

parsePdu :: Word8 -> Flags -> Size -> Get PDU
parsePdu t f s 
    | t == 1 = do
        -- Open
        time <- Timeout <$> getWord8
        _reserved <- getWord8
        _reserved <- getWord8
        _reserved <- getWord8
        (o, _) <- getOid (bigEndian f)
        d <- getDescription (bigEndian f)
        return $ Open time o d
    | t == 2 = do
        -- Close
        reason <- reasonFromTag <$> getWord8
        _reserved <- getWord8
        _reserved <- getWord8
        _reserved <- getWord8
        return $ Close reason
    | t == 3 = do
        -- Register
        context <- getContext (bigEndian f) (nonDefCont f)
        timeout <- Timeout <$> getWord8
        priority <- Priority <$> getWord8
        rsid <- RangeSubid <$> getWord8
        _reserved <- getWord8
        (oid, _) <- getOid (bigEndian f)
        case rsid of
             RangeSubid 0x00 -> return $ Register context timeout priority rsid oid Nothing
             _ -> Register context timeout priority rsid oid . Just <$> UpperBound <$> get32 (bigEndian f)
    | t == 4 = do
        -- Unregister
        context <- getContext (bigEndian f) (nonDefCont f)  
        priority <- Priority <$> getWord8
        rsid <- RangeSubid <$> getWord8
        _reserved <- getWord8
        (oid, _) <- getOid (bigEndian f)
        case rsid of
             RangeSubid 0x00 -> return $ Unregister context priority rsid oid Nothing
             _ -> Unregister context priority rsid oid . Just <$> UpperBound <$> get32 (bigEndian f)
    | t == 5 = do
        -- Get
        context <- getContext (bigEndian f) (nonDefCont f)
        oil <- getOidList (bigEndian f) []
        return $ Get context oil
    | t == 6 = do
        -- GetNext
        context <- getContext (bigEndian f) (nonDefCont f)
        srl <- getSearchRangeList (bigEndian f) []
        return $ GetNext context srl
    | t == 7 = do
        -- GetBulk
        context <- getContext (bigEndian f) (nonDefCont f)
        nonRepeaters <- NonRepeaters <$> get16 (bigEndian f)
        maxRepeaters <- MaxRepeaters <$> get16 (bigEndian f)
        srl <- getSearchRangeList (bigEndian f) []
        return $ GetBulk context nonRepeaters maxRepeaters srl
    | t == 8 = do
        -- TestSet
        context <- getContext (bigEndian f) (nonDefCont f)
        vbl <- getVarBindList (bigEndian f) []
        return $ TestSet context vbl
    | t == 9 = return $ CommitSet 
    | t == 10 = return $ UndoSet
    | t == 11 = return $ CleanupSet
    | t == 12 = do
        _c <- getContext (bigEndian f) (nonDefCont f)
        -- Notify
        context <- getContext (bigEndian f) (nonDefCont f)
        vbl <- getVarBindList (bigEndian f) []
        return $ Notify context vbl
    | t == 13 = do
        -- Ping
        context <- getContext (bigEndian f) (nonDefCont f)
        return $ Ping context
    | t == 14 = do
        -- IndexAllocate
        context <- getContext (bigEndian f) (nonDefCont f)
        vbl <- getVarBindList (bigEndian f) []
        return $ IndexAllocate context vbl
    | t == 15 = do
        -- IndexDeallocate
        context <- getContext (bigEndian f) (nonDefCont f)
        vbl <- getVarBindList (bigEndian f) []
        return $ IndexDeallocate context vbl
    | t == 16 = do
        -- AddAgentCaps
        context <- getContext (bigEndian f) (nonDefCont f)
        (oi, _) <- getOid (bigEndian f)
        description <- getDescription (bigEndian f)
        return $ AddAgentCaps context oi description
    | t == 17 = do
        -- RemoveAgentCaps
        context <- getContext (bigEndian f) (nonDefCont f)
        (oi, _) <- getOid (bigEndian f)
        return $ RemoveAgentCaps context oi
    | t == 18 = do
        -- Response
        sysuptime <- SysUptime <$> get32 (bigEndian f) 
        rerror <- rerrorFromTag <$> get16 (bigEndian f) 
        index <- Index <$> get16 (bigEndian f) 
        if (s == 8)
           then return $ Response sysuptime rerror index []
           else Response sysuptime rerror index <$> (getVarBindList (bigEndian f) [])
    | otherwise = error "parse pdu unknown tag"

bigEndian :: Flags -> Bool
bigEndian (Flags _ _ _ _ x) = x

nonDefCont :: Flags -> NonDefaultContext
nonDefCont (Flags _ _ _ x _) = x

sessionId :: Packet -> SessionID
sessionId (Packet _ _ _ x _ _) = x

sysUptime :: Packet -> SysUptime
sysUptime (Packet _ (Response x _ _ _) _ _ _ _) = x
sysUptime _ = error "sysUptime bad case"

getBodySizeFromHeader :: BL.ByteString -> Int64
getBodySizeFromHeader "" = 0
getBodySizeFromHeader bs =
    let flags = flagsFromTag (BL.index bs 2)
        bo = bigEndian flags
        s = BL.drop 16 bs
    in fromIntegral $ runGet (get32 bo) s

