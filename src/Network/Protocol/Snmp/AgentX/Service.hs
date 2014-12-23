{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Protocol.Snmp.AgentX.Service where

import Network.Socket hiding (recv, recvFrom)
import Control.Concurrent (threadDelay)
import Data.ByteString.Char8 (pack, ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Binary (encode, decode)
import Data.Monoid ((<>))
import Control.Applicative
import Control.Monad.State
import Control.Exception
-- import qualified Data.Foldable as F
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO hiding (getContents)
import Pipes.Network.TCP hiding (connect)
import Pipes

-- import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.Protocol hiding (getValue)
import Network.Protocol.Snmp.AgentX.MIBTree
-- import Network.Protocol.Snmp.AgentX.Handlers
import Network.Protocol.Snmp.AgentX.Types
-- import Debug.Trace
--
import Prelude hiding (getContents)

agent :: String -> MIBTree -> IO ()
agent path tree = bracket (openSocket path)
                          close
                          (runAgent tree)
                              
openSocket :: String -> IO Socket
openSocket path = socket AF_UNIX Stream 0 >>= \x -> connect x (SockAddrUnix path) >> return x

runAgent :: MIBTree -> Socket -> IO ()
runAgent tree socket'  = do
    hSetBuffering stdout LineBuffering
    s <- getSysUptime
    let st = ST s (PacketID 1) (toZipper tree) socket'
    evalStateT (register ) st

run :: s -> Effect (StateT s IO) a -> IO a
run st = flip evalStateT st . runEffect

register :: AgentT ()
register = do
    s <- get
    let sock' = sock s
    st <- get
    let outputP = output sock'
        inputP = input sock'
    liftIO $ run st $ do
        open >-> outputP
        liftIO $ print "open ok"
        inputP >-> sidFromOpen >-> registerP >-> outputP
        inputP >-> showResp
    liftIO $ threadDelay 100000000

showResp = do
    i <- decode . fromStrict <$> await
    liftIO $ print (i :: Packet)
    showResp

input :: Socket -> Producer ByteString (StateT ST IO)  ()
input s = fromSocket s 4096

output :: Socket -> Consumer ByteString (StateT ST IO) ()
output s = toSocket s

open :: Producer ByteString (StateT ST IO) ()
open = do
    s <- get
    let tree = fst (mibs s)
        base = head $ toList tree 
        open' = Open (Timeout 200) (oid base) (Description $ "Haskell AgentX sub-aagent: " <> pack (name base))
    yield $ toStrict (encode $ Packet 1 open' (Flags False False False False False) (SessionID 0) (TransactionID 0) (PacketID 1))

sidFromOpen :: Pipe ByteString Packet (StateT ST IO) ()
sidFromOpen = do
    i <- decode . fromStrict <$> await
    liftIO $ print i
    yield i


registerP :: Pipe Packet ByteString (StateT ST IO) ()
registerP = do
    s <- get
    response <- await
    liftIO $ print response
    let tree = toList $ fst (mibs s)
        sid = getSid response
        tid = getTid response
        pid = getPid response
        flags = Flags False False False False False
    each $ map (\x -> toStrict $ encode $ Packet 1 (mibToRegisterPdu x) flags sid tid pid) tree
    where
      getPid (Packet _ _ _ _ _ x) = x
      getTid (Packet _ _ _ _ x _) = x
      getSid (Packet _ _ _ x _ _) = x
      mibToRegisterPdu :: MIB -> PDU
      mibToRegisterPdu m = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) (oid m) Nothing

--    liftIO $ sendAll sock' (encode $ Packet 1 open (Flags False False False False False) (SessionID 0) (TransactionID 0) (PacketID 1))
--    response <- liftIO $ recvPacket sock'
--    let sid = getSid response
--        tid = getTid response
--        pid = getPid response
--    F.mapM_ (registerAll sock' sid tid pid) $ toList tree
--    where
--      registerAll sock' sid tid pid values = do
--          let p = nextPID pid
--          liftIO $ sendAll sock' (encode $ Packet 1 (mibToRegisterPdu values) (Flags False False False False False) sid tid p)
--          response <- liftIO $ recvPacket sock'
--          liftIO $ print response
--      getPid (Packet _ _ _ _ _ x) = x
--      getTid (Packet _ _ _ _ x _) = x
--      getSid (Packet _ _ _ x _ _) = x
--      mibToRegisterPdu :: MIB -> PDU
--      mibToRegisterPdu m = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) (oid m) Nothing

{--
loop :: AgentT ()
loop = forever $ do
    s <- sock <$> get
    h <- liftIO $ recv s 20
    b <- liftIO $ recv s (getBodySizeFromHeader h)
    st <- get
    liftIO . forkIO $ (flip  evalStateT) st $ do
        response <- route (decode $ h <> b) -- need catch here
        -- liftIO $ sendAll s (encode response)
        undefined


nextPID :: PacketID -> PacketID
nextPID (PacketID x) = PacketID (succ x)

--}
getSysUptime :: IO SysUptime
getSysUptime = do
    (t :: Integer) <- flip div' 1 <$> getPOSIXTime
    return $ SysUptime $ fromIntegral t
