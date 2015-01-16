{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Network.Protocol.Snmp.AgentX.Service 
( agent )
where

import Network.Socket (close, Socket, socket, Family(AF_UNIX), SocketType(Stream), connect, SockAddr(SockAddrUnix))
import Network.Socket.ByteString.Lazy (recv, send)
import Control.Concurrent (killThread, threadDelay, ThreadId)
import Data.ByteString.Char8 (pack)
import Data.Binary (encode, decode)
import Data.Monoid ((<>))
import Control.Applicative
import Control.Monad.State.Strict
import Control.Exception
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import Pipes.Concurrent (spawn, forkIO, Buffer( Unbounded ), fromInput, toOutput)
import Pipes
import Pipes.Lift
import Data.IORef
import Data.Maybe
import Prelude hiding (filter)

-- import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.Protocol hiding (getValue)
import Network.Protocol.Snmp.AgentX.MIBTree
import Network.Protocol.Snmp.AgentX.Handlers
import Network.Protocol.Snmp.AgentX.Types
-- import Debug.Trace
--

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
    i <- newIORef Nothing
    p <- newIORef (PacketID 1)
    let st = ST s p (toZipper tree) socket' i
    (reqTo, req) <- (\(x,y) -> (toOutput x, fromInput y)) <$> spawn Unbounded
    (respTo, resp) <- (\(x,y) -> (toOutput x, fromInput y)) <$> spawn Unbounded
    sortPid <- fiber st $ input >-> sortInput reqTo respTo
    run st $ resp >-> registrator >-> output
    serverPid <-  fiber st $ req >-> server
    agentPid <- fiber st $ forever $ do
        resp >-> client ping >-> output
        liftIO $ threadDelay 5000000
    _ <-  getLine :: IO String
    void $ mapM (liftIO . killThread) [serverPid, agentPid, sortPid] -- p2,p3,p4,p5, 

---------------------------------------------------------------------------
-- Pipes eval 
---------------------------------------------------------------------------
run :: ST -> Effect AgentT a -> IO a
run s eff = runEffect $ evalStateP s eff

server :: Consumer Packet AgentT ()
server = forever $ do
    m <- await
    get >>= flip fiber (yield m >-> server' >-> output)

fiber :: MonadIO m => ST -> Effect AgentT () -> m ThreadId
fiber st = liftIO . forkIO . run st

input :: Producer Packet AgentT ()
input = forever $ do
    sock' <- sock <$> get
    h <- liftIO $ recv sock' 20
    b <- liftIO $ recv sock' (getBodySizeFromHeader h)
    yield $ decode $ h <> b

output :: Consumer Packet AgentT ()
output = forever $ do
    sock' <- sock <$> get
    bs <- await
    void . liftIO $ send sock' (encode bs)

sortInput :: Consumer Packet AgentT () -> Consumer Packet AgentT () -> Consumer Packet AgentT ()
sortInput requests responses = forever $ await >>= sortInput'
    where
        sortInput' m@(Packet _ Response{} _ _ _ _) = yield m >-> responses
        sortInput' m@_ = yield m >-> requests

server' :: Pipe Packet Packet AgentT ()
server' = forever $ await >>= lift . route >>= yieldOnlyJust 
    where
        yieldOnlyJust Nothing = return ()
        yieldOnlyJust (Just resp) = yield resp

registrator :: Pipe Packet Packet AgentT () 
registrator = do
    openPacket <- lift open
    -- open session
    client openPacket
    -- register mibs
    registerPackages <- lift register
    mapM_ client registerPackages

client :: Packet -> Pipe Packet Packet AgentT ()
client p = do
    pid <- lift getPid
    sid <- lift getSid
    yield (setPidSid p pid sid) 
    resp <- await
    sessionsRef <- sessions <$> get
    sid' <- liftIO $ readIORef sessionsRef
    maybe (lift . setSid . gs $ resp) (const $ return ()) sid'
    -- liftIO $ print $ "response: " <> show p
    where
      setPidSid (Packet a b c _ e _) pid sid = Packet a b c sid e pid
      gs (Packet _ _ _ s _ _) = s

_dp :: String -> Pipe Packet Packet AgentT ()
_dp label = forever $ do
    i <- await
    liftIO $ print $ label ++ " " ++ show i
    yield i

---------------------------------------------------------------------------
-- AgentT eval 
---------------------------------------------------------------------------
register :: AgentT [Packet]
register = do
    s <- get
    let tree = toList $ fst (mibs s)
        flags = Flags False False False False False
        sid = SessionID 0
        tid = TransactionID 0
        pid = PacketID 0
    return $ map (\x -> Packet 1 (mibToRegisterPdu x) flags sid tid pid) $ tree
    where
      mibToRegisterPdu :: MIB -> PDU
      mibToRegisterPdu m = Register Nothing (Timeout 0) (Priority 127) (RangeSubid 0) (oid m) Nothing

open :: AgentT Packet
open = do
    base <- head . toList . fst . mibs <$> get
    let open' = Open (Timeout 0) (oid base) (Description $ "Haskell AgentX sub-aagent: " <> pack (name base))
    return $ Packet 1 open' (Flags False False False False False) (SessionID 0) (TransactionID 0) (PacketID 0)

ping :: Packet
ping = Packet 1 (Ping Nothing) (Flags False False False False False) (SessionID 0) (TransactionID 0) (PacketID 0)

getSid :: AgentT SessionID
getSid = do
    sesRef <- sessions <$> get
    s <- liftIO $ readIORef sesRef
    return $ fromMaybe (SessionID 0) s

setSid :: SessionID -> AgentT ()
setSid sid = do
    sesRef <- sessions <$> get
    liftIO $ atomicModifyIORef' sesRef $ \_ -> (Just sid, ())
    liftIO $ print $ "set sid " ++ show sid
    

getPid :: AgentT PacketID
getPid = do
    pidRef <- packetCounter <$> get
    liftIO $ atomicModifyIORef' pidRef $ \x -> (succ x, succ x)

getSysUptime :: IO SysUptime
getSysUptime = do
    (t :: Integer) <- flip div' 1 <$> getPOSIXTime
    return $ SysUptime $ fromIntegral t
