{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Network.Protocol.Snmp.AgentX.Packet.Put 
( putPacket
) where

import Data.Binary.Put (putBuilder, Put)
import Data.Binary.Builder
import Data.Word
import Control.Applicative hiding (empty)
import Data.Monoid
import Data.Bits.Bitwise (fromListLE, toListLE)
import Control.Monad.State.Strict hiding (gets)
import qualified Control.Monad.State.Strict as S
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Label
import Data.Label.Monadic
import qualified Data.Label as DL
import qualified Data.Label.Monadic as DLM
import Control.Category ((.), id)
import Prelude hiding ((.), id)

import Network.Protocol.Snmp.AgentX.Packet.Types
import Network.Protocol.Snmp (Value(..), OID)


type Pack = State ST

putPacket :: Packet -> Put
putPacket p = putBuilder . evalState pack $ ST p empty empty 0 

pack :: Pack Builder
pack = do
    b <- packBody
    h <- packHeader
    s <- packSize
    return $ h <> s <> b

packHeader :: Pack Builder
packHeader = undefined

packBody :: Pack Builder
packBody = undefined

packSize :: Pack Builder 
packSize = undefined

fixSize :: Word32 -> Pack ()
fixSize x = DLM.modify bodySize (+ x)

packWord :: (w -> Builder) -> (w -> Builder) -> w -> Pack Builder
packWord be le w = do
    b <-  gets ( nonDefaultContext . flags . packet )
    if b then return (be w) else return (le w)

pack64 :: Word64 -> Pack Builder
pack64 = packWord putWord64be putWord64le 

pack32 :: Word32 -> Pack Builder
pack32 = packWord putWord32be putWord32le 

pack16 :: Word16 -> Pack Builder
pack16 = packWord putWord16be putWord16le 

packFlags :: Pack Builder
packFlags = do
    Flags a b c d e <-  gets (flags . packet )
    return $ singleton $ fromListLE [a, b, c, d, e]

packBool :: Bool -> Pack Builder
packBool True  = return $ singleton 1
packBool False = return $ singleton 0


class BodyBuilder a where
    packsz :: a -> Pack Builder

instance BodyBuilder Word64 where
    packsz w = fixSize 8 >> packWord putWord64be putWord64le w

instance BodyBuilder Word32 where
    packsz w = fixSize 4 >> packWord putWord32be putWord32le w

instance BodyBuilder Word16 where
    packsz w = fixSize 2 >> packWord putWord16be putWord16le w

instance BodyBuilder Value where
    packsz (Integer x) = packsz (fromIntegral x :: Word32)
    packsz (Counter32 x) = packsz x 
    packsz (Counter64 x) = packsz x
    packsz (Gaude32 x) = packsz x
    packsz (TimeTicks x) = packsz x
    packsz (OI x) = packOID False x
    packsz (String x) = packsz x
    packsz (Opaque x) = packsz x
    packsz (IpAddress a b c d) = packsz $ B.pack [a,b,c,d]
    packsz _ = return empty

instance BodyBuilder ByteString where
    packsz bs = do
        s <- packsz bsLen
        fixSize bsLen
        fixSize tailLen
        return $ s <> fromByteString bs <> tailB
        where
        bsLen = fromIntegral $ B.length bs
        tailB = fromByteString $ B.replicate (fromIntegral tailLen) 0x00
        tailLen :: Word32
        tailLen = (4 - bsLen `rem` 4) `rem` 4

instance BodyBuilder Integer where
    packsz x = packsz (fromIntegral x :: Word32)

instance BodyBuilder VarBind where
    packsz (VarBind o v) = do
        t <-  packsz (tag v :: Word16)
        z <-  packsz (0 :: Word16)
        oi <- packOID False o
        val <-  packsz v
        return $ t <> z <> oi <> val

type Include = Bool

packOID :: Include -> OID -> Pack Builder
packOID _ [] = fixSize 4 >> return (singleton 0 <> singleton 0 <> singleton 0 <> singleton 0)
packOID i xs@(1:3:6:1:[]) = do -- Not clearly in rfc!!!
    include' <- packBool i
    fixSize 4
    oi <- mapM packsz xs
    return $ singleton 4 <> singleton 0 <> include' <> singleton 0 <> mconcat oi
packOID i (1:3:6:1:ls) = do
    include' <- packBool i
    fixSize 4
    oi <- mapM packsz (tail ls)
    return $ singleton (fromIntegral (length ls - 1)) <> singleton (fromIntegral (head ls)) <> include' <> singleton 0 <> mconcat oi
packOID i xs = do
    include' <- packBool i
    fixSize 34
    oi <- mapM packsz xs
    return $ singleton (fromIntegral (length xs)) <> singleton 0 <> include' <> singleton 0 <> mconcat oi


{--

instance ToBuilder (Packet -> Builder) where
    toBuilder (Packet _ p f@(Flags _ _ _ _ nbo) (SessionID sid) (TransactionID tid) (PacketID pid)) =
        let header = singleton 1 <> singleton (pduToTag p) <> toBuilder (fixContextFlags p f) <> singleton 0
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
pduToBuilder (Register mc (Timeout t) (Priority p) (RangeSubid r) s mu) (Flags _ _ _ _ bi) =  --TODO ? INSTANCE_REGISTRATION
    let upperBound = case (r, mu) of
                          (0, _) -> empty
                          (_, Just (UpperBound u)) -> builder32 bi u
                          (_, Nothing) -> error "pduToBuilder"
        body = toBuilder bi mc 
            <> singleton t <> singleton p <> singleton r <> singleton 0
            <> toBuilder bi False s
            <> upperBound
    in (body, bodyLength body)
pduToBuilder (Unregister mc (Priority p) (RangeSubid r) s mu) (Flags _ _ _ _ bi)  =
    let upperBound = case (r, mu) of
                          (0, _) -> empty
                          (_, Just (UpperBound u)) -> builder32 bi u
                          (_, Nothing) -> error "pduToBuilder"
        body = toBuilder bi mc
            <> singleton 0 <> singleton p <> singleton r <> singleton 0
            <> toBuilder bi False s
            <> upperBound
    in (body, bodyLength body)
pduToBuilder (Get mc sr) (Flags _ _ _ _ bi)  =
    let searchRangeList = Prelude.foldr (\a b -> toBuilder bi True a <> toBuilder bi False ([] :: OID)  <> b) empty sr
        body = toBuilder bi mc  <> searchRangeList
    in (body, bodyLength body)
pduToBuilder (GetNext mc sr) (Flags _ _ _ _ bi)  =
    let searchRangeList = Prelude.foldr (\(SearchRange (s,e, _)) b -> toBuilder bi True s <> toBuilder bi False e <> b) empty sr
        body = toBuilder bi mc <> searchRangeList
    in (body, bodyLength body)
pduToBuilder (GetBulk mc (NonRepeaters nr) (MaxRepeaters mr) sr) (Flags _ _ _ _ bi)  =
    let searchRangeList = Prelude.foldr (\(SearchRange (s,e, _)) b -> toBuilder bi True s <> toBuilder bi False e <> b) empty sr
        body = toBuilder bi mc
            <> builder16 bi nr <> builder16 bi mr
            <> searchRangeList
    in (body, bodyLength body)
pduToBuilder (TestSet mc vb) (Flags _ _ _ _ bi)  =
    let vbl = Prelude.foldr (\a b -> toBuilder bi a <> b) empty vb
        body = toBuilder bi mc <> vbl
    in (body, bodyLength body)
pduToBuilder CommitSet _  = (empty, PayloadLenght 0)
pduToBuilder UndoSet _  = (empty, PayloadLenght 0)
pduToBuilder CleanupSet _  = (empty, PayloadLenght 0)
pduToBuilder (Notify mc vb) (Flags _ _ _ _ bi)  =
    let body = toBuilder bi mc <> Prelude.foldr (\a b -> toBuilder bi a <> b) empty vb
    in (body, bodyLength body)
pduToBuilder (Ping mc) (Flags _ _ _ _ bi)  =
    let body = toBuilder bi mc
    in (body, bodyLength body)
pduToBuilder (IndexAllocate mc vb) (Flags _ _ _ _ bi)  =
    let body = toBuilder bi mc <> Prelude.foldr (\a b -> toBuilder bi a <> b) empty vb
    in (body, bodyLength body)
pduToBuilder (IndexDeallocate mc vb) (Flags _ _ _ _ bi)  =
    let body = toBuilder bi mc <> Prelude.foldr (\a b -> toBuilder bi a <> b) empty vb
    in (body, bodyLength body)
pduToBuilder (AddAgentCaps mc o (Description d)) (Flags _ _ _ _ bi)  =
    let body = toBuilder bi mc <> toBuilder bi False o <> toBuilder bi d
    in (body, bodyLength body)
pduToBuilder (RemoveAgentCaps mc o) (Flags _ _ _ _ bi)  =
    let body = toBuilder bi mc <> toBuilder bi False o
    in (body, bodyLength body)
pduToBuilder (Response (SysUptime su) re (Index i) vb) (Flags _ _ _ _ bi)  =
    let body = builder32 bi su
            <> builder16 bi (rerrorToTag re) <> builder16 bi i
            <> Prelude.foldr (\a b -> toBuilder bi a <> b) empty vb
    in (body, bodyLength body)

bodyLength :: Builder -> PayloadLenght
bodyLength = PayloadLenght . fromIntegral . BL.length . toLazyByteString

instance ToBuilder (BigEndian -> Maybe Context -> Builder) where
    toBuilder bi (Just (Context c)) = toBuilder bi c
    toBuilder _ Nothing = empty -- error "uncorrected flag, or context"

bodySizeFromHeader :: BL.ByteString -> Int64
bodySizeFromHeader "" = 0
bodySizeFromHeader bs =
    let flags = flagsFromTag (BL.index bs 2)
        bo = bigEndian flags
        s = BL.drop 16 bs
    in fromIntegral $ runGet (get32 bo) s
fixContextFlags :: PDU -> Flags -> Flags
fixContextFlags = fixContextFlags' . pduToContext 
  where
    fixContextFlags' :: Maybe Context -> Flags -> Flags
    fixContextFlags' Nothing (Flags a b c _ d) = Flags a b c False d
    fixContextFlags' _ (Flags a b c _ d) = Flags a b c True d
    pduToContext :: PDU -> Maybe Context
    pduToContext (Register x _ _ _ _ _) = x
    pduToContext (Unregister x _ _ _ _) = x
    pduToContext (Get x _ ) = x
    pduToContext (GetNext x _ ) = x
    pduToContext (GetBulk x _ _ _ ) = x
    pduToContext (TestSet x _ ) = x
    pduToContext (Notify x _ ) = x
    pduToContext (Ping x ) = x
    pduToContext (IndexAllocate x _) = x
    pduToContext (IndexDeallocate x _) = x
    pduToContext (AddAgentCaps x _ _) = x
    pduToContext (RemoveAgentCaps x _ ) = x
    pduToContext _ = Nothing
--}
