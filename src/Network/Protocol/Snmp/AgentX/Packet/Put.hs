{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Network.Protocol.Snmp.AgentX.Packet.Put 
( putPacket
) where

import Data.Binary.Put (putBuilder, Put)
import Data.Binary.Builder
import Data.Word
import Data.Monoid
import Data.Bits.Bitwise (fromListLE)
import Control.Monad.State.Strict hiding (gets)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Label.Monadic
import qualified Data.Label as DL
import qualified Data.Label.Monadic as DLM
import Control.Category ((.))
import Prelude hiding ((.))

import Network.Protocol.Snmp.AgentX.Packet.Types
import Network.Protocol.Snmp (Value(..), OID)

type Pack = State ST

putPacket :: Packet -> Put
putPacket p = putBuilder . evalState pack $ ST p 0 
       
packBody :: Pack Builder 
packBody = packsz =<< DLM.gets (pdu . packet)

pack :: Pack Builder
pack = do
    !b <-  packBody
    !s <-  packSize
    p <-  DLM.gets (pdu . packet)
    f <-  packFlags
    SessionID sid' <- DLM.gets (sid . packet)
    PacketID pid' <- DLM.gets (pid . packet)
    TransactionID tid' <- DLM.gets (tid . packet)
    psid <- pack32 sid'
    ppid <- pack32 pid'
    ptid <- pack32 tid'
    return $ singleton 1 <> singleton (tag p) <> f <> singleton 0 
          <> psid <> ptid <> ppid <> s <> b

packSize :: Pack Builder 
packSize = do
    s <- DLM.gets bodySize
    pack32 s 


fixSize :: Word32 -> Pack ()
fixSize x = DLM.modify bodySize (+ x)

packWord :: (w -> Builder) -> (w -> Builder) -> w -> Pack Builder
packWord be le w = do
    b <-  gets ( bigEndian . flags . packet )
    if b then return (be w) else return (le w)

pack32 :: Word32 -> Pack Builder
pack32 = packWord putWord32be putWord32le 

packFlags :: Pack Builder
packFlags = do
    Flags a b c d e <-  gets (flags . packet )
    return $ singleton $ fromListLE [a, b, c, d, e]

packBool :: Bool -> Pack Builder
packBool True  = return $ singleton 1
packBool False = return $ singleton 0


class SizedBuilder a where
    packsz :: a -> Pack Builder

instance SizedBuilder Word64 where
    packsz w = fixSize 8 >> packWord putWord64be putWord64le w

instance SizedBuilder Word32 where
    packsz w = fixSize 4 >> packWord putWord32be putWord32le w

instance SizedBuilder Word16 where
    packsz w = fixSize 2 >> packWord putWord16be putWord16le w

instance SizedBuilder Value where
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

instance SizedBuilder ByteString where
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

instance SizedBuilder Integer where
    packsz x = packsz (fromIntegral x :: Word32)

instance SizedBuilder VarBind where
    packsz (VarBind o v) = do
        t <-  packsz (tag v :: Word16)
        z <-  packsz (0 :: Word16)
        oi <- packOID False o
        val <-  packsz v
        return $ t <> z <> oi <> val

instance SizedBuilder (Maybe Context) where
    packsz Nothing = fixContextFlags Nothing >> return empty
    packsz mc@(Just (Context c)) = fixContextFlags mc >> packsz c

instance SizedBuilder SearchRange where
    packsz x = do
        s <- packOID True (DL.get startOID x)
        e <- packOID False (DL.get endOID x)
        return $ s <> e

fixContextFlags :: Maybe Context -> Pack ()
fixContextFlags Nothing = DLM.modify (nonDefaultContext . flags . packet) (const False)
fixContextFlags _       = DLM.modify (nonDefaultContext . flags . packet) (const True)

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
    fixSize 4
    oi <- mapM packsz xs
    return $ singleton (fromIntegral (length xs)) <> singleton 0 <> include' <> singleton 0 <> mconcat oi

instance SizedBuilder PDU where
    packsz (Open t o d) = do
        poid         <-  packOID False o
        pdescription <-  packsz d 
        fixSize 4
        fixContextFlags Nothing
        return $ (singleton t) <> singleton 0 <> singleton 0 <> singleton 0 <> poid <> pdescription
    packsz (Close er) = do
        fixSize 4
        fixContextFlags Nothing
        return $ singleton (tag er) <> singleton 0 <> singleton 0 <> singleton 0
    packsz (Register mc t p rs oi mu) = do
        pcontext <- packsz mc
        up <-  case (rs, mu) of
                     (0, _) -> return empty
                     (_, Just x) -> packsz x
                     _ -> error "packPacket Register"
        poid <-  packOID False oi
        fixSize 4
        return $ pcontext <> singleton t <> singleton p <> singleton rs <> singleton 0 <> poid <> up
    packsz (Unregister mc p rs oi mu) = do
        pcontext <- packsz mc
        up <- case (rs, mu) of
                    (0, _) -> return empty
                    (_, Just x) -> packsz x
                    _ -> error "packPacket Unregister"
        poid <-  packOID False oi
        fixSize 4
        return $ pcontext <> singleton 0 <> singleton p <> singleton rs <> singleton 0 <> poid <> up
    packsz (Get mc []) = do
        pcontext <-  packsz mc
        return $ pcontext 
    packsz (Get mc (x:xs)) = do
        pcontext <- packsz mc
        y <- packOID True x
        ys <-  mapM (packOID False) xs
        return $ pcontext <> y <> mconcat ys
    packsz (GetNext mc xs) = do
        pcontext <- packsz mc
        ys <- mapM packsz xs
        return $ pcontext <> mconcat ys
    packsz (GetBulk mc nr mr xs) = do
        pcontext <- packsz mc
        nonrepeaters <- packsz nr
        maxrepeaters <- packsz mr
        ys <- mapM packsz xs
        return $ pcontext <> nonrepeaters <> maxrepeaters <> mconcat ys
    packsz (TestSet mc xs) = do
        pcontext <- packsz mc
        ys <- mapM packsz xs
        return $ pcontext <> mconcat ys
    packsz CommitSet = fixContextFlags Nothing >>  return empty
    packsz UndoSet = fixContextFlags Nothing >>  return empty
    packsz CleanupSet = fixContextFlags Nothing >>  return empty
    packsz (Notify mc xs) = do
        pcontext <- packsz mc
        ys <- mapM packsz xs
        return $ pcontext <> mconcat ys
    packsz (Ping mc) = packsz mc
    packsz (IndexAllocate mc xs) = do
        pcontext <- packsz mc
        ys <- mapM packsz xs
        return $ pcontext <> mconcat ys
    packsz (IndexDeallocate mc xs) = do
        pcontext <- packsz mc
        ys <- mapM packsz xs
        return $ pcontext <> mconcat ys
    packsz (AddAgentCaps mc o d) = do
        pcontext <- packsz mc
        oi <-  packOID False o
        pdescription <- packsz d
        return $ pcontext <> oi <> pdescription
    packsz (RemoveAgentCaps mc o) = do
        pcontext <- packsz mc
        oi <- packOID False o
        return $ pcontext <> oi
    packsz (Response s re i xs) = do
        psysuptime <- packsz s
        perror    <-  packsz (tag re :: Word16)
        pindex     <- packsz i
        ys <- mapM packsz xs
        fixContextFlags Nothing
        return $ psysuptime <> perror <> pindex <> mconcat ys
