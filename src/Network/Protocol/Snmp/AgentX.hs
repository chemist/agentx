{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.Snmp.AgentX where

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Types hiding (getValue)
import Network.Protocol.Snmp.AgentX.Monads
import Network.Protocol.Snmp.AgentX.MIBTree
import Data.Tree
import Network.Info
import qualified Network.Info as NI
import Data.ByteString.Char8 (pack)
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Applicative
import qualified Data.Tree.Zipper as Zip
import Data.IORef
import Debug.Trace
import qualified Data.Foldable as F
import Control.Monad.State (evalStateT)
import Control.Exception (catch)

import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy
import Data.Binary
import Data.Monoid

fixmon :: IO (MIBTree MIB)
fixmon = do
    time' <-  time
    interfaces' <- interfaces
    return $ fromList $ 
      [ iso
      , org
      , dod
      , internet
      , private
      , enterprise
      , mkModule 44729 "enterprise" "Fixmon"
      , mkObject 0 "Fixmon" "about" Fixed
      , mkObjectType 0 "about" "name" (String "fixmon snmp agent") Fixed
      , mkObjectType 1 "about" "version" (Integer 1) Fixed
      ] <> time' <> interfaces'
    where
    interfaces :: IO [MIB]
    interfaces = do
        nx <- getNetworkInterfaces
        let xs = zip [0 .. fromIntegral $ length nx -1] nx
            indexes = flip map xs $ \(i,_) -> mkObjectType i "indexes" "index" (Integer . fromIntegral $ i) Fixed
            names = flip map xs $ \(i, o) -> mkObjectType i "names" "name" (String . pack . NI.name $ o) Fixed
            ipv4s = flip map xs $ \(i, o) -> mkObjectType i "ipv4s" "ipv4" (String . pack . show . NI.ipv4 $ o) Fixed
            ipv6s = flip map xs $ \(i, o) -> mkObjectType i "ipv6s" "ipv6" (String . pack . show . NI.ipv6 $ o) Fixed
            macs = flip map xs $ \(i, o) -> mkObjectType i "macs" "mac" (String . pack . show . NI.mac $ o) Fixed
        return $ 
            mkObject 2 "Fixmon" "interfaces" (Read interfaces) :
              (mkObject 0 "interfaces" "indexes" Fixed : indexes) <>
              (mkObject 1 "interfaces" "names" Fixed   : names )  <>
              (mkObject 2 "interfaces" "ipv4s" Fixed   : ipv4s )  <>
              (mkObject 3 "interfaces" "ipv6s" Fixed   : ipv6s )  <>
              (mkObject 4 "interfaces" "macs" Fixed    : macs )

time :: IO [MIB]
time = do
    t <- flip div' 1 <$> getPOSIXTime
    return $ 
        [ mkObject 1 "Fixmon" "time" (Read time)
        , mkObjectType 0 "time" "description" (String "sysUptime") Fixed
        , mkObjectType 1 "time" "now"  (TimeTicks (fromIntegral t)) Fixed
        ]

getSysUptime :: IO SysUptime
getSysUptime = do
    t <- flip div' 1 <$> getPOSIXTime
    return $ SysUptime $ fromIntegral t

{--

genSave :: IORef Value -> IO ATree
genSave io = do
    x <- readIORef io
    return $ leaf 2 "ioref" x (ReadWrite (genSave io) (saveIO io))

saveIO :: IORef Value -> ATree -> IO ()
saveIO io (Node (Values _ _ v _) _ ) = atomicWriteIORef io v

genSaveMul :: IORef Value -> IORef Value -> IO ATree
genSaveMul n s = do
    nx <- readIORef n
    sx <- readIORef s
    let baseLeaf = leaf 3 "multi" Zero (ReadOnly (genSaveMul n s))
    return $ root baseLeaf $ 
      [ leaf 0 "int" nx (WriteOnly (saveIO n))
      , leaf 1 "str" sx (WriteOnly (saveIO s))
      ]
 
saveValue :: Value -> (ATree -> IO ()) -> IO ()
saveValue v f = f (Node (Values undefined undefined v undefined) undefined) 
--}

check :: IO ()
check = do
    tree <- fixmon
    sock <- socket AF_UNIX Stream 0
    connect sock (SockAddrUnix "/var/agentx/master")
    let open = Open (Timeout 200) [1,3,6,1,4,1,44729] (Description "Haskell AgentX sub-aagent")
    -- open
    sendAll sock (encode $ Packet 1 open (Flags False False False False False) (SessionID 0) (TransactionID 0) (PacketID 1))
    -- for lazy wait more big chunk then i have, and here i fix size for recv
    response <- recvPacket sock
    print "open session"
    print response
    sid <- newIORef $ getSid response
    tid <- newIORef $ getTid response
    pid <- newIORef $ getPid response
    print "register all mibs"
    F.mapM_ (registerAll sock sid tid pid) $ toList tree
    responder tree sock sid tid pid
    where
      registerAll sock sid tid pid values = do
          s <- readIORef sid
          t <-  readIORef tid 
          p <-  atomicModifyIORef pid $ \x -> (succ x, succ x)
          sendAll sock (encode $ Packet 1 (mibToRegisterPdu values) (Flags False False False False False) (SessionID s) (TransactionID t) (PacketID p))
          response <- recvPacket sock
          print response

      getPid (Packet _ _ _ _ _ (PacketID x)) = x
      getTid (Packet _ _ _ _ (TransactionID x) _) = x
      getSid (Packet _ _ _ (SessionID x) _ _) = x

responder tree sock sid tid pid = do
    packet <- recvPacket sock
    case  packet of
         Packet _ (Get mc [oid]) f si ti pi -> doResponse tree sock oid si ti pi f
         _ -> print packet
    responder tree sock sid tid pid

doResponse :: MIBTree MIB -> Socket -> [Integer] -> SessionID -> TransactionID -> PacketID -> Flags -> IO ()
doResponse tree sock oid si ti pi f = do
    let value = case find oid tree of
                   Nothing  -> NoSuchObject 
                   Just v -> getValue v
    now <- getSysUptime
    let pdu = Response now NoAgentXError (Index 0) [VarBind oid value]
        response = Packet 1 pdu f si ti pi
    sendAll sock (encode response)
    print $ "send response " ++ show response

mibToRegisterPdu :: MIB -> PDU
mibToRegisterPdu m = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) (oid m) Nothing

recvPacket :: Socket -> IO Packet
recvPacket sock = do
    h <- recv sock 20
    b <- recv sock (getBodySizeFromHeader h)
    return $ decode (h <> b)

