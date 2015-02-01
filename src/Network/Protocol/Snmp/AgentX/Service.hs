{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Network.Protocol.Snmp.AgentX.Service 
( agent )
where

import Network.Socket (close, Socket, socket, Family(AF_UNIX), SocketType(Stream), connect, SockAddr(SockAddrUnix))
import Network.Socket.ByteString.Lazy (recv, send)
import Control.Concurrent (killThread, threadDelay, ThreadId)
import Data.Binary (encode, decode)
import Data.Monoid ((<>))
import Data.String (fromString)
import Control.Applicative hiding (empty)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Exception
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import Pipes.Concurrent (spawn, forkIO, Buffer( Unbounded ), fromInput, toOutput)
import Pipes
import Pipes.Lift
import Control.Concurrent.MVar
import Data.Maybe
import qualified Data.Map.Strict as Map 
import qualified Data.Label as DL
import Prelude 

-- import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.Packet 
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
    s <- newMVar =<< getSysUptime
    i <- newEmptyMVar
    p <- newMVar minBound
    m <- newMVar (toZipper tree)
    ts <- newMVar Map.empty
    let st = ST s p m socket' i ts
    (reqTo, req) <- (\(x,y) -> (toOutput x, fromInput y)) <$> spawn Unbounded
    (respTo, resp) <- (\(x,y) -> (toOutput x, fromInput y)) <$> spawn Unbounded
    sortPid <- fiber st $ input >-> sortInput reqTo respTo
    run st $ resp >-> _dp "r" >-> registrator >-> _dp "ro" >->  output
    serverPid <-  fiber st $ req >-> _dp "in" >-> server
    agentPid <- fiber st $ forever $ do
        resp >-> client ping >-> output
        liftIO $ threadDelay 5000000
    _ <-  getLine :: IO String
    void $ mapM (liftIO . killThread) [serverPid, sortPid, agentPid] -- p2,p3,p4,p5, 

---------------------------------------------------------------------------
-- Pipes eval 
---------------------------------------------------------------------------
run :: ST -> Effect AgentT a -> IO a
run s eff = runEffect $ runReaderP s eff

server :: Consumer Packet AgentT ()
server = forever $ do
    m <- await
    ask >>= flip fiber (yield m >-> server' >-> _dp "out" >-> output)

fiber :: MonadIO m => ST -> Effect AgentT () -> m ThreadId
fiber st = liftIO . forkIO . run st

input :: Producer Packet AgentT ()
input = forever $ do
    sock' <- sock <$> ask
    h <- liftIO $ recv sock' 20
    b <- liftIO $ recv sock' (bodySizeFromHeader h)
    yield $ decode $ h <> b

output :: Consumer Packet AgentT ()
output = forever $ do
    sock' <- sock <$> ask
    bs <- await
    void . liftIO $ send sock' (encode bs)

sortInput :: Consumer Packet AgentT () -> Consumer Packet AgentT () -> Consumer Packet AgentT ()
sortInput requests responses = forever $ await >>= sortInput'
    where
        sortInput' m 
          | isResponse m = yield m >-> responses
          | otherwise    = yield m >-> requests
        isResponse p = case DL.get pdu p of
                            Response{} -> True
                            _ -> False

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
    pid' <- lift getPid
    sid' <- lift getSid
    yield $ DL.set pid pid' (DL.set sid sid' p)
    resp <- await
    sessionsRef <- sessions <$> ask
    sid'' <- liftIO $ tryReadMVar sessionsRef
    maybe (lift . setSid . (DL.get sid) $ resp) (const $ return ()) sid''
    -- liftIO $ print $ "response: " <> show p

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
    s <- mibs <$> ask
    zipper' <- liftIO $ readMVar s
    let tree = toList $ fst zipper'
    return $ map (\x -> mkPacket (mibToRegisterPdu x) minBound minBound minBound) $ filter isObjectType tree
        where
        mibToRegisterPdu :: MIB -> PDU
        mibToRegisterPdu m = Register Nothing minBound (toEnum 127) minBound (oid m) Nothing

open :: AgentT Packet
open = do
    m <- mibs <$> ask
    base <- head . toList . fst <$> (liftIO $ readMVar m)
    let open' = Open minBound (oid base) ("Haskell AgentX sub-aagent: " <> fromString (name base))
    return $ mkPacket open' minBound minBound minBound 

ping :: Packet
ping = mkPacket (Ping Nothing) minBound minBound minBound 

getSid :: AgentT SessionID
getSid = do
    sesRef <- sessions <$> ask
    s <- liftIO $ tryReadMVar sesRef
    return $ fromMaybe minBound s

setSid :: SessionID -> AgentT ()
setSid sid' = do
    sesRef <- sessions <$> ask
    liftIO $ putMVar sesRef sid'
    liftIO $ print $ "set sid " ++ show sid'
    

getPid :: AgentT PacketID
getPid = do
    pidRef <- packetCounter <$> ask
    liftIO $ modifyMVar pidRef $ \x -> return (succ x, succ x)

getSysUptime :: IO SysUptime
getSysUptime = do
    (t :: Integer) <- flip div' 1 <$> getPOSIXTime
    return . toEnum . fromIntegral $ t
