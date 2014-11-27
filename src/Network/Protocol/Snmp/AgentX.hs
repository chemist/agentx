{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.Snmp.AgentX where

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Types
import Network.Protocol.Snmp.AgentX.Monads
import Network.Protocol.Snmp.AgentX.ATree
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

fixmon :: IO MIBTree
fixmon = do
    time' <- time 
    interfaces' <- interfaces
    return $ mModule [1,3,6,1,4,1,44729] "enterprise" "Fixmon"
      [ mObject 0 "Fixmon" "about" Fixed about
      , time'
      , interfaces'
      ]
    where
    about :: MIBForest
    about = [ mObjectType 0 "about" "name" (String "fixmon snmp agent") Fixed
            , mObjectType 1 "about" "version" (Integer 1) Fixed
            ]
    time :: IO MIBTree
    time = do
        t <- flip div' 1 <$> getPOSIXTime
        return $ mObject 1 "Fixmon" "time" (Read time)
            [ mObjectType 0 "time" "description" (String "sysUptime") Fixed
            , mObjectType 1 "time" "now"  (TimeTicks (fromIntegral t)) Fixed
            ]
    interfaces :: IO MIBTree
    interfaces = do
        nx <- getNetworkInterfaces
        let xs = zip [0 .. fromIntegral $ length nx -1] nx
            indexes = flip map xs $ \(i,_) -> mObjectType i "indexes" "index" (Integer . fromIntegral $ i) Fixed
            names = flip map xs $ \(i, o) -> mObjectType i "names" "name" (String . pack . NI.name $ o) Fixed
            ipv4s = flip map xs $ \(i, o) -> mObjectType i "ipv4s" "ipv4" (String . pack . show . NI.ipv4 $ o) Fixed
            ipv6s = flip map xs $ \(i, o) -> mObjectType i "ipv6s" "ipv6" (String . pack . show . NI.ipv6 $ o) Fixed
            macs = flip map xs $ \(i, o) -> mObjectType i "macs" "mac" (String . pack . show . NI.mac $ o) Fixed
        return $ mObject 2 "Fixmon" "interfaces" (Read interfaces) 
            [ mObject 0 "interfaces" "indexes" Fixed indexes
            , mObject 1 "interfaces" "names" Fixed names
            , mObject 2 "interfaces" "ipv4s" Fixed ipv4s
            , mObject 3 "interfaces" "ipv6s" Fixed ipv6s
            , mObject 4 "interfaces" "macs" Fixed macs
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
    F.mapM_ (registerAll sock sid tid pid) $ fullOidTree tree
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

doResponse :: MIBTree -> Socket -> [Integer] -> SessionID -> TransactionID -> PacketID -> Flags -> IO ()
doResponse tree sock oid si ti pi f = do
    (_, founded) <- catch (evalStateT (find oid) (Zip.fromTree tree)) catchFindErrors
    let value = case founded of
                   Object{} -> NoSuchObject 
                   ObjectType _ _ _ _ value _ -> value
    now <- getSysUptime
    let pdu = Response now NoAgentXError (Index 0) [VarBind oid value]
        response = Packet 1 pdu f si ti pi
    sendAll sock (encode response)
    print $ "send response " ++ show response

catchFindErrors :: FindE -> IO (OID, MIB)
catchFindErrors _ = return ([], ObjectType undefined  undefined undefined undefined NoSuchObject undefined)



recvPacket :: Socket -> IO Packet
recvPacket sock = do
    h <- recv sock 20
    b <- recv sock (getBodySizeFromHeader h)
    return $ decode (h <> b)

