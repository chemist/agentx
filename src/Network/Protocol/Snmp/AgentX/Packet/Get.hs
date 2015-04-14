{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.Snmp.AgentX.Packet.Get 
( getPacket
, bodySizeFromHeader
)
where

import Network.Protocol.Snmp.AgentX.Packet.Types ( bigEndian
                                                 , nonDefaultContext
                                                 , unTag
                                                 , Flags(..)
                                                 , PDU(..)
                                                 , VarBind(..)
                                                 , Context(..)
                                                 , SearchRange(..) 
                                                 , Packet(..)
                                                 , PacketID(..)
                                                 , TransactionID(..)
                                                 , SessionID(..)
                                                 )
import Network.Protocol.Snmp (Value(..), OID)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary hiding (get)
import Data.Binary.Get 
import Data.Bits.Bitwise (toListLE)
import Data.Monoid ((<>))
import Control.Applicative hiding (empty)
import Data.Int
import Data.Label

getPacket :: Get Packet
getPacket = do
    version <- getWord8 
    pduTag <- getWord8
    flags <- decodeFlags <$> getWord8
    _reserved <- getWord8
    sid <- get32 flags 
    tid <- get32 flags 
    pid <- get32 flags 
    bodySize <- get32 flags 
    pdu <- parsePdu pduTag flags bodySize
    return $ Packet version pdu flags (SessionID sid) (TransactionID tid) (PacketID pid)

decodeFlags :: Word8 -> Flags
decodeFlags x =
    let (i:n:a:nd:nb:_) = toListLE x
    in Flags i n a nd nb

get16 :: Flags -> Get Word16
get16 f = case get bigEndian f of
               True -> getWord16be
               False -> getWord16le

get32 :: Flags -> Get Word32
get32 f = case get bigEndian f of
               True -> getWord32be
               False -> getWord32le

get64 :: Flags -> Get Word64
get64 f = case get bigEndian f of
               True -> getWord64be
               False -> getWord64le

type Size = Word32
type Include   = Bool

getBool :: Get Bool
getBool = do
    x <- getWord8  
    case x of
         1 -> return True
         0 -> return False
         _ -> error "bad getBool"

getOid :: Flags -> Get (OID, Include)
getOid flags = do
    nSubId <- getWord8
    prefix <- getWord8
    include <- getBool
    _reserved <- getWord8
    end <- sequence $ replicate (fromIntegral nSubId) (get32 flags)
    case (nSubId, prefix) of
         (0, 0) -> return ([], include)
         (_, 0) -> return (map fromIntegral end, include)
         (_, x) -> return ([1,3,6,1] <> map fromIntegral (fromIntegral x:end), include)

getString :: Flags -> Get ByteString
getString bo = do
    l <- fromIntegral <$> get32 bo
    let fullLength = l + (4 - l `rem` 4) `rem` 4
    s <- getByteString fullLength
    return $ B.take l s

getContext :: Flags -> Get (Maybe Context)
getContext f = case (get nonDefaultContext f) of
                    False -> return Nothing
                    True -> Just <$> Context <$> getString f

getOidList :: Flags -> [OID] -> Get [OID]
getOidList bo xs = do
    (oi, _) <- getOid bo
    isEnd <- isEmpty
    if isEnd 
       then return $ reverse (addOi oi xs)
       else getOidList bo (addOi oi xs)
    where
    addOi [] xss = xss
    addOi oi xss = oi:xss

getSearchRange :: Flags -> Get SearchRange
getSearchRange bo = do
    (first, include) <- getOid bo
    (second, _) <- getOid bo
    return $ SearchRange first second include

getSearchRangeList :: Flags -> [SearchRange] -> Get [SearchRange]
getSearchRangeList bo xs = do
    sr <- getSearchRange bo
    isEnd <- isEmpty
    if isEnd
       then return $ reverse (sr:xs)
       else getSearchRangeList bo (sr:xs)

getValue :: Flags -> Word16 -> Get Value
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

getVarBind :: Flags -> Get VarBind
getVarBind bo = do
    valueTag <- get16 bo 
    _reserved <- getWord16be
    (oi, _) <- getOid bo
    v <- getValue bo valueTag 
    return $ VarBind oi v

getVarBindList :: Flags -> [VarBind] -> Get [VarBind]
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
        time <-  getWord8
        _reserved <- getWord8
        _reserved <- getWord8
        _reserved <- getWord8
        (o, _) <- getOid f
        d <- getString f
        return $ Open time o d
    | t == 2 = do
        -- Close
        reason <- unTag <$> getWord8
        _reserved <- getWord8
        _reserved <- getWord8
        _reserved <- getWord8
        return $ Close reason
    | t == 3 = do
        -- Register
        context <- getContext f
        timeout <-  getWord8
        priority <-  getWord8
        rsid <- getWord8
        _reserved <- getWord8
        (oid, _) <- getOid f
        case rsid of
             0x00 -> return $ Register context timeout priority rsid oid Nothing
             _ -> Register context timeout priority rsid oid . Just <$> get32 f
    | t == 4 = do
        -- Unregister
        context <- getContext f
        priority <-  getWord8
        rsid <-  getWord8
        _reserved <- getWord8
        (oid, _) <- getOid f
        case rsid of
             0x00 -> return $ Unregister context priority rsid oid Nothing
             _ -> Unregister context priority rsid oid . Just <$> get32 f
    | t == 5 = do
        -- Get
        context <- getContext f
        oil <- getOidList f []
        return $ Get context oil
    | t == 6 = do
        -- GetNext
        context <- getContext f
        srl <- getSearchRangeList f []
        return $ GetNext context srl
    | t == 7 = do
        -- GetBulk
        context <- getContext f
        nonRepeaters <- get16 f
        maxRepeaters <- get16 f
        srl <- getSearchRangeList f []
        return $ GetBulk context nonRepeaters maxRepeaters srl
    | t == 8 = do
        -- TestSet
        context <- getContext f
        vbl <- getVarBindList f []
        return $ TestSet context vbl
    | t == 9 = return $ CommitSet 
    | t == 10 = return $ UndoSet
    | t == 11 = return $ CleanupSet
    | t == 12 = do
        -- Notify
        context <- getContext f
        vbl <- getVarBindList f []
        return $ Notify context vbl
    | t == 13 = do
        -- Ping
        context <- getContext f
        return $ Ping context
    | t == 14 = do
        -- IndexAllocate
        context <- getContext f
        vbl <- getVarBindList f []
        return $ IndexAllocate context vbl
    | t == 15 = do
        -- IndexDeallocate
        context <- getContext f
        vbl <- getVarBindList f []
        return $ IndexDeallocate context vbl
    | t == 16 = do
        -- AddAgentCaps
        context <- getContext f
        (oi, _) <- getOid f
        description <-  getString f
        return $ AddAgentCaps context oi description
    | t == 17 = do
        -- RemoveAgentCaps
        context <- getContext f
        (oi, _) <- getOid f
        return $ RemoveAgentCaps context oi
    | t == 18 = do
        -- Response
        sysuptime <-  get32 f 
        rerror <- unTag <$> get16 f 
        index <- get16 f 
        if (s == 8)
           then return $ Response sysuptime rerror index []
           else Response sysuptime rerror index <$> (getVarBindList f [])
    | otherwise = error "parse pdu unknown tag"


-- | get body size from header
bodySizeFromHeader :: BL.ByteString -> Int64
bodySizeFromHeader "" = 0
bodySizeFromHeader bs =
    let flags = decodeFlags (BL.index bs 2)
        s = BL.drop 16 bs
    in fromIntegral $ runGet (get32 flags) s




