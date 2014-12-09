{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Protocol.Snmp.AgentX.Monads where

import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Binary (encode, decode)
import Data.Monoid ((<>))
import Control.Applicative
import Control.Monad.State
import Control.Exception
import qualified Data.Foldable as F
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Types hiding (getValue)
import Network.Protocol.Snmp.AgentX.MIBTree
import Debug.Trace

data ST = ST
  { sysuptime :: SysUptime
  , packetCounter :: PacketID
  , mibs :: Zipper MIB
  , sock :: Socket
  }

type AgentT = StateT ST IO

newtype MibT a = MibT {unMibT :: AgentT a} deriving (Monad, MonadIO, Functor, Applicative)

instance MonadState (Zipper MIB) MibT where
    get = MibT $ do
        x <- get
        return $ (mibs x)
    put x = MibT $ do
        ST s p _ so <- get
        put $ ST s p x so

recvPacket :: Socket -> IO Packet
recvPacket sock = do
    h <- recv sock 20
    b <- recv sock (getBodySizeFromHeader h)
    return $ decode (h <> b)


route :: Packet -> AgentT Packet
route p@(Packet _ pdu _ _ _ _) = do
    liftIO $ print pdu
    case pdu of
         Get _ oids -> getHandler oids p
         _ -> undefined

getHandler :: [OID] -> Packet -> AgentT Packet
getHandler oids p = do
    pdu <- makePdu =<< getHandler' oids
    let (Packet v _ flags sid tid pid) = p
    return $ Packet v pdu flags sid tid pid
    where
        getHandler' :: [OID] -> AgentT [MIB]
        getHandler' [] = return []
        getHandler' (x:xs) = do
            z <- mibs <$> get
            (:) <$> (liftIO $ evalStateT (findR x) z) <*> getHandler' xs
--        getH :: [OID] -> AgentT [MIB]
--        getH (x:xs) = (:) <$> lift (findR x) <*> getH xs


makePdu :: [MIB] -> AgentT PDU
makePdu xs = do
  now <- sysuptime <$> get
  return $ Response now NoAgentXError (Index 0) $ map mibToVarBind xs

mibToVarBind :: MIB -> VarBind
mibToVarBind y = VarBind (oid y) (getValue y)

agent :: String -> MIBTree MIB -> IO ()
agent path tree = bracket (openSocket path)
                          close
                          (runSnmp tree)
                              
openSocket :: String -> IO Socket
openSocket path = do
    sock <- socket AF_UNIX Stream 0
    connect sock (SockAddrUnix path)
    return sock

loop :: AgentT ()
loop = forever $ do
    s <- sock <$> get
    request <- liftIO $ recvPacket s
    response <- route request -- need catch here
    liftIO $ sendAll s (encode response)

register :: AgentT ()
register = do
    s <- get
    let tree = fst (mibs s)
        base = head $ toList tree
        open = Open (Timeout 200) (oid base) (Description $ "Haskell AgentX sub-aagent: " <> pack (name base))
        sock' = sock s
    liftIO $ sendAll sock' (encode $ Packet 1 open (Flags False False False False False) (SessionID 0) (TransactionID 0) (PacketID 1))
    response <- liftIO $ recvPacket sock'
    let sid = getSid response
        tid = getTid response
        pid = getPid response
    F.mapM_ (registerAll sock' sid tid pid) $ toList tree
    where
      registerAll sock' sid tid pid values = do
          let p = nextPID pid
          liftIO $ sendAll sock' (encode $ Packet 1 (mibToRegisterPdu values) (Flags False False False False False) sid tid p)
          response <- liftIO $ recvPacket sock'
          liftIO $ print response
      getPid (Packet _ _ _ _ _ x) = x
      getTid (Packet _ _ _ _ x _) = x
      getSid (Packet _ _ _ x _ _) = x


runSnmp :: MIBTree MIB -> Socket -> IO ()
runSnmp tree sock = do
    s <- getSysUptime
    let st = ST s (PacketID 1) (toZipper tree) sock
    evalStateT (register >> loop) st

nextPID :: PacketID -> PacketID
nextPID (PacketID x) = PacketID (succ x)

getSysUptime :: IO SysUptime
getSysUptime = do
    t <- flip div' 1 <$> getPOSIXTime
    return $ SysUptime $ fromIntegral t

mibToRegisterPdu :: MIB -> PDU
mibToRegisterPdu m = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) (oid m) Nothing

