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
import Control.Applicative hiding (empty)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Exception
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import Pipes.Concurrent (spawn, forkIO, fromInput, toOutput, unbounded)
import Pipes
import Pipes.Lift
import Control.Concurrent.MVar
import Data.Maybe
import Data.Default
import qualified Data.Map.Strict as Map 
import qualified Data.Label as DL

import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.Packet 
import Network.Protocol.Snmp.AgentX.MIBTree hiding (register)
import qualified Network.Protocol.Snmp.AgentX.MIBTree as MIBTree
import Network.Protocol.Snmp.AgentX.Handlers
import Network.Protocol.Snmp.AgentX.Types

-- | start agent
agent :: FilePath  -- ^ path to socket
      -> OID -- ^ base oid
      -> [MIB] -- ^ MIBs
      -> IO ()
agent path o tree = bracket (openSocket path)
                            close
                            (runAgent o tree)
                              
openSocket :: String -> IO Socket
openSocket path = socket AF_UNIX Stream 0 >>= \x -> connect x (SockAddrUnix path) >> return x

runAgent :: OID -> [MIB] -> Socket -> IO ()
runAgent modOid tree socket'  = do
    hSetBuffering stdout LineBuffering
    -- make state
    st <- initAgent modOid tree socket'
    -- start timer
    timer <- forkIO $ modifyMVar_ (sysuptime st) (const $ getSysUptime) >> threadDelay 1000000
    -- spawn mailboxes for requests and for responses
    (reqTo, req) <- (\(x,y) -> (toOutput x, fromInput y)) <$> spawn unbounded
    (respTo, resp) <- (\(x,y) -> (toOutput x, fromInput y)) <$> spawn unbounded
    -- spawn sort machine 
    sortPid <- fiber st $ input >-> sortInput reqTo respTo
    -- open session 
    run st $ resp >-> openSession >-> output
    -- start register, unregister fiber
    regPid <- fiber st $ resp >-> registrator >->  output
    -- start server fiber
    serverPid <-  fiber st $ req >-> server
    -- start agent fiber
    agentPid <- fiber st $ forever $ do
        resp >-> client ping >-> output
        liftIO $ threadDelay 5000000
    regPidTimer <- registerTimer st
    _ <-  getLine :: IO String
    liftIO $ putStrLn "unregister all MIB"
    evalStateT unregisterFullTree =<< readMVar (mibs st)
    liftIO $ threadDelay 1000000
    void $ mapM (liftIO . killThread) [serverPid, sortPid, agentPid, regPid, timer, regPidTimer] -- p2,p3,p4,p5, 

initAgent :: OID -> [MIB] -> Socket -> IO SubAgentState
initAgent modOid tree socket' = do
    sysUptimeMVar <- newMVar =<< getSysUptime
    sessionsMVar <- newEmptyMVar
    packetCounterMVar <- newMVar minBound
    mod' <- mkModule modOid tree 
    moduleMVar <- newMVar =<< flip execStateT mod' (initModule >> registerFullTree) 
    transactionsMVar <- newMVar Map.empty
    return $ SubAgentState sysUptimeMVar packetCounterMVar moduleMVar socket' sessionsMVar transactionsMVar

registerTimer :: SubAgentState -> IO ThreadId
registerTimer st = forkIO . forever $ do
    oldTree <- evalStateT askTree =<< readMVar (mibs st)
    liftIO $ threadDelay 1000000
    evalStateT (flip regByDiff oldTree =<< askTree) =<< readMVar (mibs st)
---------------------------------------------------------------------------
-- Pipes eval 
---------------------------------------------------------------------------
run :: SubAgentState -> Effect SubAgent a -> IO a
run s eff = runEffect $ runReaderP s eff

server :: Consumer Packet SubAgent ()
server = forever $ do
    m <- await
    -- ask >>= flip fiber (yield m >-> server' >-> _dp "out" >-> output)
    ask >>= flip fiber (yield m >-> server' >-> output)

fiber :: MonadIO m => SubAgentState -> Effect SubAgent () -> m ThreadId
fiber st = liftIO . forkIO . run st

input :: Producer Packet SubAgent ()
input = forever $ do
    sock' <- sock <$> ask
    h <- liftIO $ recv sock' 20
    b <- liftIO $ recv sock' (bodySizeFromHeader h)
    yield $ decode $ h <> b

output :: Consumer Packet SubAgent ()
output = forever $ do
    sock' <- sock <$> ask
    bs <- await
    void . liftIO $ send sock' (encode bs)

sortInput :: Consumer Packet SubAgent () -> Consumer Packet SubAgent () -> Consumer Packet SubAgent ()
sortInput requests responses = forever $ await >>= sortInput'
    where
        sortInput' m 
          | isResponse m = yield m >-> responses
          | otherwise    = yield m >-> requests
        isResponse p = case DL.get pdu p of
                            Response{} -> True
                            _ -> False

server' :: Pipe Packet Packet SubAgent ()
server' = forever $ await >>= lift . route >>= yieldOnlyJust 
    where
        yieldOnlyJust Nothing = return ()
        yieldOnlyJust (Just resp) = yield resp

openSession :: Pipe Packet Packet SubAgent ()
openSession = do
    openPacket <- lift open
    client openPacket

registrator :: Pipe Packet Packet SubAgent () 
registrator = forever $ mapM_ client =<< lift register

client :: Packet -> Pipe Packet Packet SubAgent ()
client p = do
    pid' <- lift getPid
    sid' <- lift getSid
    yield $ DL.set pid pid' (DL.set sid sid' p)
    resp <- await
    sessionsRef <- sessions <$> ask
    sid'' <- liftIO $ tryReadMVar sessionsRef
    maybe (lift . setSid . (DL.get sid) $ resp) (const $ return ()) sid''

_dp :: String -> Pipe Packet Packet SubAgent ()
_dp label = forever $ do
    i <- await
    liftIO $ print $ label ++ " " ++ show i
    yield i

---------------------------------------------------------------------------
-- SubAgent eval 
---------------------------------------------------------------------------
register :: SubAgent [Packet]
register = do
    s <- mibs <$> ask
    m  <- liftIO $ readMVar s
    (toReg, toUnReg) <- liftIO $ takeMVar (DL.get MIBTree.register m)
    liftIO $ mapM_ (\x -> print ("R " ++ show x)) toReg
    liftIO $ mapM_ (\x -> print ("UR " ++ show x)) toUnReg
    return $ map toRegPacket toReg <> map toUnRegPacket toUnReg
        where
        toRegPacket :: (OID, Maybe Context) -> Packet
        toRegPacket m =
            let pduList = Register (snd m) minBound (toEnum 127) minBound (fst m) Nothing
            in mkPacket def pduList def minBound minBound minBound
        toUnRegPacket :: (OID, Maybe Context) -> Packet
        toUnRegPacket m =
            let pduList = Unregister (snd m) (toEnum 127) minBound (fst m) Nothing
            in mkPacket def pduList def  minBound minBound minBound

open :: SubAgent Packet
open = do
    s <- mibs <$> ask
    m <- liftIO $ readMVar s
    let open' = Open minBound (DL.get moduleOID m) ("Haskell AgentX sub-aagent")
    return $ mkPacket def open' def minBound minBound minBound 

ping :: Packet
ping = mkPacket def (Ping Nothing) def minBound minBound minBound 

getSid :: SubAgent SessionID
getSid = do
    sesRef <- sessions <$> ask
    s <- liftIO $ tryReadMVar sesRef
    return $ fromMaybe minBound s

setSid :: SessionID -> SubAgent ()
setSid sid' = do
    sesRef <- sessions <$> ask
    liftIO $ putMVar sesRef sid'
    liftIO $ print $ "set sid " ++ show sid'
    

getPid :: SubAgent PacketID
getPid = do
    pidRef <- packetCounter <$> ask
    liftIO $ modifyMVar pidRef $ \x -> return (succ x, succ x)

getSysUptime :: IO SysUptime
getSysUptime = do
    (t :: Integer) <- flip div' 1 <$> getPOSIXTime
    return . toEnum . fromIntegral $ t
