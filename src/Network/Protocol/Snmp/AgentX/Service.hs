{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Network.Protocol.Snmp.AgentX.Service 
( agent 
, runAgent
, Client(..) 
, request
, requestWithResponse
)
where

import Network.Socket (close, Socket, socket, Family(AF_UNIX), SocketType(Stream), connect, SockAddr(SockAddrUnix))
import Network.Socket.ByteString.Lazy (recv, send)
import Control.Concurrent (killThread, threadDelay, ThreadId, myThreadId)
import Data.Binary (encode, decode)
import Data.Monoid ((<>))
import Control.Applicative hiding (empty)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Exception
import Data.IORef
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import Pipes.Concurrent (spawn, forkIO, fromInput, toOutput, unbounded)
import Pipes
import Pipes.Lift
import Control.Concurrent.MVar
import Data.Default
import qualified Data.Map.Strict as Map 
import qualified Data.Label as DL
import System.Exit
import System.Posix.Signals

import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.Packet 
import Network.Protocol.Snmp.AgentX.MIBTree hiding (register)
import qualified Network.Protocol.Snmp.AgentX.MIBTree as MIBTree
import Network.Protocol.Snmp.AgentX.Handlers
import Network.Protocol.Snmp.AgentX.Types

-- | start agent
agent :: FilePath  -- ^ path to socket
      -> OID -- ^ base oid
      -> Maybe Client -- ^ client
      -> [MIB] -- ^ MIBs
      -> IO ()
agent path o client tree = bracket (openSocket path)
                                   close
                                   (runAgent o tree client)
                              
openSocket :: String -> IO Socket
openSocket path = socket AF_UNIX Stream 0 >>= \x -> connect x (SockAddrUnix path) >> return x

-- | start agent with socket
-- exit when catch sigQUIT, sigTERM, keyboardSignal
-- show MIB tree when catch sigUSR1
runAgent :: OID -> [MIB] -> Maybe Client -> Socket -> IO ()
runAgent modOid tree mclient socket' = do
    hSetBuffering stdout LineBuffering
    -- make state
    st <- initAgent modOid tree socket'
    -- start timer
    timer <- forkIO $ do
        t <- getSysUptime
        atomicWriteIORef (sysuptime st) t 
        threadDelay 1000000
    -- spawn mailboxes for requests and for responses
    (reqTo, req) <- (\(x,y) -> (toOutput x, fromInput y)) <$> spawn unbounded
    (respTo, resp) <- (\(x,y) -> (toOutput x, fromInput y)) <$> spawn unbounded
    -- spawn sort machine 
    sortPid <- fiber st $ input >-> sortInput reqTo respTo
    -- open session 
    run st $ resp >-> openSession >-> output
    -- start register, unregister fiber
    regPid <- fiber st $ resp >-> registrator >->  output
    -- start server fibers
    serverPid <-  fiber st $ req >-> server
    serverPid1 <-  fiber st $ req >-> server
    serverPid2 <-  fiber st $ req >-> server
    -- start agent fiber
    agentPid <- fiber st $ resp >-> runClient (maybe def id mclient) >-> output
    regPidTimer <- registerTimer st
    mainPid <- myThreadId
    let stopAgent = do
            liftIO $ putStrLn "unregister all MIB"
            evalStateT unregisterFullTree =<< readIORef (mibs st)
            liftIO $ threadDelay 1000000
            void $ mapM (liftIO . killThread) [serverPid, serverPid1, serverPid2, sortPid, agentPid, regPid, timer, regPidTimer] 
            throwTo mainPid ExitSuccess
    void $ installHandler keyboardSignal (Catch stopAgent) Nothing
    void $ installHandler sigTERM (Catch stopAgent) Nothing
    void $ installHandler sigQUIT (Catch stopAgent) Nothing
    void $ installHandler sigUSR1 (Catch (print =<< readIORef (mibs st))) Nothing
    forever $ threadDelay 10000000

initAgent :: OID -> [MIB] -> Socket -> IO SubAgentState
initAgent modOid tree socket' = do
    sysUptimeIORef <- newIORef =<< getSysUptime
    sessionsIORef <- newIORef Nothing
    packetCounterIORef <- newIORef minBound
    mod' <- mkModule modOid tree 
    moduleIORef <- newIORef =<< flip execStateT mod' (initModule >> registerFullTree) 
    m <- newMVar ()
    transactionsIORef <- newIORef Map.empty
    return $ SubAgentState sysUptimeIORef packetCounterIORef moduleIORef m socket' sessionsIORef transactionsIORef

registerTimer :: SubAgentState -> IO ThreadId
registerTimer st = forkIO . forever $ do
    oldTree <- evalStateT askTree =<< readIORef (mibs st)
    liftIO $ threadDelay 1000000
    evalStateT (flip regByDiff oldTree =<< askTree) =<< readIORef (mibs st)
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
    request openPacket

registrator :: Pipe Packet Packet SubAgent () 
registrator = forever $ mapM_ request =<< lift register

-- | simple request for client, if you dont need response.
request :: Packet -> Pipe Packet Packet SubAgent ()
request p = do
    pid' <- lift getPid
    sid' <- lift getSid
    yield $ DL.set pid pid' (DL.set sid sid' p)
    resp <- await
    lift . setSid . (DL.get sid) $ resp

-- | as request, but you can work with response.
requestWithResponse :: Packet -> Pipe Packet Packet SubAgent Packet
requestWithResponse p = do
    pid' <- lift getPid
    sid' <- lift getSid
    yield $ DL.set pid pid' (DL.set sid sid' p)
    resp <- await
    lift . setSid . (DL.get sid) $ resp
    return resp

-- | if you need client
newtype Client = Client { runClient :: Pipe Packet Packet SubAgent () }

-- | by default just ping every 5s.
instance Default Client where
    def = Client . forever $ do
      request ping
      liftIO $ threadDelay 5000000
    
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
    m  <- liftIO $ readIORef s
    (toReg, toUnReg) <- liftIO $ takeMVar (DL.get MIBTree.register m)
--    liftIO $ mapM_ (\x -> print ("R " ++ show x)) toReg
--    liftIO $ mapM_ (\x -> print ("UR " ++ show x)) toUnReg
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
    m <- liftIO $ readIORef s
    let open' = Open minBound (DL.get moduleOID m) ("Haskell AgentX sub-aagent")
    return $ mkPacket def open' def minBound minBound minBound 

ping :: Packet
ping = mkPacket def (Ping Nothing) def minBound minBound minBound 

getSid :: SubAgent SessionID
getSid = do
    sesRef <- sessions <$> ask
    s <- liftIO $ readIORef sesRef
    maybe (return minBound) return s

setSid :: SessionID -> SubAgent ()
setSid sid' = do
    sesRef <- sessions <$> ask
    s <- liftIO $ readIORef sesRef
    case s of
         Nothing -> liftIO $ atomicModifyIORef' sesRef (const $ (Just sid', ()))
         Just x 
           | x == sid' -> return ()
           | otherwise -> liftIO $ atomicModifyIORef' sesRef (const $ (Just sid', ()))
    

getPid :: SubAgent PacketID
getPid = do
    pidRef <- packetCounter <$> ask
    liftIO $ atomicModifyIORef' pidRef $ \x -> (succ x, succ x)

getSysUptime :: IO SysUptime
getSysUptime = do
    (t :: Integer) <- flip div' 1 <$> getPOSIXTime
    return . toEnum . fromIntegral $ t
