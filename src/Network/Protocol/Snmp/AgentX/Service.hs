{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    let st = ST s (PacketID 1) (toZipper tree) socket'
    (pipeO, pipeI) <- (\(x,y) -> (toOutput x, fromInput y)) <$> spawn Unbounded
    let fiber = liftIO . forkIO . run st
    run st $ do
        open >-> output
        input >-> register >-> output
        p1 <- fiber $ pipeI >-> dropResponse >-> responder >-> output
        p2 <- fiber $ pipeI >-> dropResponse >-> responder >-> output
        p3 <- fiber $ pipeI >-> dropResponse >-> responder >-> output
        p4 <- fiber $ pipeI >-> dropResponse >-> responder >-> output
        p5 <- fiber $ pipeI >-> dropResponse >-> responder >-> output
        p6 <- fiber $ input >-> pipeO
        _ <- liftIO $ getLine :: Effect AgentT String
        void $ mapM (liftIO . killThread) [p1,p2,p3,p4,p5, p6]

run :: ST -> Effect AgentT a -> IO a
run st = flip evalStateT st . runEffect

responder :: Pipe Packet Packet AgentT ()
responder = forever $ await >>= lift . route >>= yield

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

open :: Producer Packet AgentT ()
open = do
    base <- head . toList . fst . mibs <$> get
    let open' = Open (Timeout 200) (oid base) (Description $ "Haskell AgentX sub-aagent: " <> pack (name base))
    yield $ Packet 1 open' (Flags False False False False False) (SessionID 0) (TransactionID 0) (PacketID 1)

register :: Pipe Packet Packet AgentT ()
register = do
    s <- get
    response <- await
    liftIO $ print response
    let tree = toList $ fst (mibs s)
        sid = getSid response
        tid = getTid response
        pid = getPid response
        flags = Flags False False False False False
    each $ map (\(x, p) -> Packet 1 (mibToRegisterPdu x) flags sid tid p) $ zip tree [succ pid .. ]
    where
      getPid (Packet _ _ _ _ _ x) = x
      getTid (Packet _ _ _ _ x _) = x
      getSid (Packet _ _ _ x _ _) = x
      mibToRegisterPdu :: MIB -> PDU
      mibToRegisterPdu m = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) (oid m) Nothing

getSysUptime :: IO SysUptime
getSysUptime = do
    (t :: Integer) <- flip div' 1 <$> getPOSIXTime
    return $ SysUptime $ fromIntegral t
