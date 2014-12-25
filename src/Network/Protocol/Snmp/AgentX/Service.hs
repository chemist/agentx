{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Network.Protocol.Snmp.AgentX.Service where

import Network.Socket (close, Socket, socket, Family(AF_UNIX), SocketType(Stream), connect, SockAddr(SockAddrUnix))
import Network.Socket.ByteString.Lazy (recv, send)
import Control.Concurrent (killThread)
import Data.ByteString.Char8 (pack)
import Data.Binary (encode, decode)
import Data.Monoid ((<>))
import Control.Applicative
import Control.Monad.State
import Control.Exception
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import Pipes.Concurrent (spawn, forkIO, Buffer( Unbounded ), fromInput, toOutput)
import Pipes
import Pipes.Prelude (filter)
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
    i <- newIORef (Nothing, Nothing)
    p <- newIORef (PacketID 1)
    let st = ST s p (toZipper tree) socket' i
    (pipeO, pipeI) <- (\(x,y) -> (toOutput x, fromInput y)) <$> spawn Unbounded
    let fiber = liftIO . forkIO . run st
    run st $ do
        p6 <- fiber $ input >-> pipeO
        client pipeI >-> output
        ssid <- lift getServerSid
        p1 <- fiber $ pipeI >-> filterBySid ssid >-> server >-> output
        p2 <- fiber $ pipeI >-> filterBySid ssid >-> server >-> output
        p3 <- fiber $ pipeI >-> filterBySid ssid >-> server >-> output
        p4 <- fiber $ pipeI >-> filterBySid ssid >-> server >-> output
        p5 <- fiber $ pipeI >-> filterBySid ssid >-> server >-> output
        _ <- liftIO $ getLine :: Effect AgentT String
        void $ mapM (liftIO . killThread) [p1,p2,p3,p4,p5, p6]

run :: ST -> Effect AgentT a -> IO a
run st = flip evalStateT st . runEffect

server :: Pipe Packet Packet AgentT ()
server = forever $ await >>= lift . route >>= yield

client :: Producer Packet AgentT () -> Producer Packet AgentT () 
client fromSnmpServer = do
    st <- get
    let reqResp = requestResponse fromSnmpServer
    openPacket <- lift open
    -- open 2 session
    liftIO $ run st $ reqResp openPacket
    liftIO $ run st $ reqResp openPacket
    -- register mibs
    registerPackages <- lift register
    liftIO $ run st $ mapM_ reqResp registerPackages

requestResponse :: Producer Packet AgentT () -> Packet -> Effect AgentT ()
requestResponse fromSnmpServer p = do
    pid <- lift getPid
    sid <- lift getClientSid
    yield (setPidSid p pid sid) >-> output
    fromSnmpServer >-> response 
    where
      setPidSid (Packet a b c _ e _) pid sid = Packet a b c sid e pid

filterBySid :: SessionID -> Pipe Packet Packet AgentT ()
filterBySid sid = filter (\x -> getSid x == sid)
    where
      getSid (Packet _ _ _ s _ _) = s

response :: Consumer Packet AgentT ()
response = do
    p <- await
    sessionsRef <- sessions <$> get
    (c, s) <- liftIO $ readIORef sessionsRef
    case (c, s) of
         (Nothing, Nothing) -> lift $ setClientSid (getSid p)
         (_      , Nothing) -> lift $ setServerSid (getSid p)
         _ -> return ()
    liftIO $ print $ "response: " <> show p
    where
      getSid (Packet _ _ _ s _ _) = s

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
      mibToRegisterPdu m = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) (oid m) Nothing

dropResponse :: Pipe Packet Packet AgentT ()
dropResponse = forever $ do
    p <- await
    case p of
         Packet _ Response{} _ _ _ _ -> liftIO $ print $ "Not routable " ++ show p
         _ -> yield p

dp :: String -> Pipe Packet Packet AgentT ()
dp label = forever $ do
    i <- await
    liftIO $ print $ label ++ " " ++ show i
    yield i

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

open :: AgentT Packet
open = do
    base <- head . toList . fst . mibs <$> get
    let open' = Open (Timeout 200) (oid base) (Description $ "Haskell AgentX sub-aagent: " <> pack (name base))
    return $ Packet 1 open' (Flags False False False False False) (SessionID 0) (TransactionID 0) (PacketID 0)

getClientSid :: AgentT SessionID
getClientSid = do
    sesRef <- sessions <$> get
    s <- fst <$> (liftIO $ readIORef sesRef)
    return $ fromMaybe (SessionID 0) s

getServerSid :: AgentT SessionID
getServerSid = do
    sesRef <- sessions <$> get
    s <- snd <$> (liftIO $ readIORef sesRef)
    return $ fromMaybe (SessionID 0) s

setClientSid :: SessionID -> AgentT ()
setClientSid sid = do
    sesRef <- sessions <$> get
    liftIO $ atomicModifyIORef' sesRef $ \(_,y) -> ((Just sid, y), ())
    liftIO $ print $ "set sid " ++ show sid
    liftIO $ print =<< readIORef sesRef

setServerSid :: SessionID -> AgentT ()
setServerSid sid = do
    sesRef <- sessions <$> get
    liftIO $ atomicModifyIORef' sesRef $ \(x,_) -> ((x,Just sid), ())
    liftIO $ print $ "set sid " ++ show sid
    

getPid :: AgentT PacketID
getPid = do
    pidRef <- packetCounter <$> get
    liftIO $ atomicModifyIORef' pidRef $ \x -> (succ x, succ x)

getSysUptime :: IO SysUptime
getSysUptime = do
    (t :: Integer) <- flip div' 1 <$> getPOSIXTime
    return $ SysUptime $ fromIntegral t
