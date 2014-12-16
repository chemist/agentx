{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Protocol.Snmp.AgentX.Service where

import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy
import Data.ByteString.Char8 (pack)
import Data.Binary (encode, decode)
import Data.Monoid ((<>))
import Control.Applicative
import Control.Monad.State
import Control.Exception
import qualified Data.Foldable as F
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)

-- import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.Protocol hiding (getValue)
import Network.Protocol.Snmp.AgentX.MIBTree
import Network.Protocol.Snmp.AgentX.Handlers
import Network.Protocol.Snmp.AgentX.Types
-- import Debug.Trace
--

recvPacket :: Socket -> IO Packet
recvPacket s = do
    h <- recv s 20
    b <- recv s (getBodySizeFromHeader h)
    return $ decode (h <> b)

agent :: String -> MIBTree -> IO ()
agent path tree = bracket (openSocket path)
                          close
                          (runAgent tree)
                              
openSocket :: String -> IO Socket
openSocket path = socket AF_UNIX Stream 0 >>= \x -> connect x (SockAddrUnix path) >> return x

runAgent :: MIBTree -> Socket -> IO ()
runAgent tree socket'  = do
    s <- getSysUptime
    let st = ST s (PacketID 1) (toZipper tree) socket'
    evalStateT (register >> loop) st

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
      mibToRegisterPdu :: MIB -> PDU
      mibToRegisterPdu m = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) (oid m) Nothing

loop :: AgentT ()
loop = forever $ do
    s <- sock <$> get
    request <- liftIO $ recvPacket s
    response <- route request -- need catch here
    liftIO $ sendAll s (encode response)


nextPID :: PacketID -> PacketID
nextPID (PacketID x) = PacketID (succ x)

getSysUptime :: IO SysUptime
getSysUptime = do
    (t :: Integer) <- flip div' 1 <$> getPOSIXTime
    return $ SysUptime $ fromIntegral t

