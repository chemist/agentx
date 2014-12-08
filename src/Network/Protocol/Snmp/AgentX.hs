{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Control.Monad.State (evalStateT, StateT)
import Control.Exception (catch, throwIO)

import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy
import Data.Binary
import Data.Monoid

fixmon :: IO (MIBTree MIB)
fixmon = do
    time' <-  time
    interfaces' <- interfaces
    io <-  newIORef (Integer 0)
    str <-  newIORef (String "hello")
    io1 <-  newIORef (Integer 1)
    str1 <-  newIORef (String "bye")
    let base = [ ([1,3,6,1,4,1,44729,3,0], io)
               , ([1,3,6,1,4,1,44729,3,1], io)
               , ([1,3,6,1,4,1,44729,4,0], io)
               , ([1,3,6,1,4,1,44729,4,1], io)
               ]
    io' <- saved io str base
    otherS <- otherSaved io1 str1 base
    return $ fromList $ 
      [ iso
      , org
      , dod
      , internet
      , private
      , enterprise
      , mkModule 44729 "enterprise" "Fixmon"
      , mkObject 0 "Fixmon" "about" Fixed
      , mkObjectType 0 "about" "name" (String "fixmon snmp agent")
      , mkObjectType 1 "about" "version" (Integer 1)
      ] <> time' <> interfaces' <> io' <> otherS
    where
    interfaces :: IO [MIB]
    interfaces = do
        nx <- getNetworkInterfaces
        let xs = zip [0 .. fromIntegral $ length nx -1] nx
            indexes = flip map xs $ \(i,_) -> mkObjectType i "indexes" "index" (Integer . fromIntegral $ i)
            names = flip map xs $ \(i, o) -> mkObjectType i "names" "name" (String . pack . NI.name $ o)
            ipv4s = flip map xs $ \(i, o) -> mkObjectType i "ipv4s" "ipv4" (String . pack . show . NI.ipv4 $ o)
            ipv6s = flip map xs $ \(i, o) -> mkObjectType i "ipv6s" "ipv6" (String . pack . show . NI.ipv6 $ o)
            macs = flip map xs $ \(i, o) -> mkObjectType i "macs" "mac" (String . pack . show . NI.mac $ o)
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
        , mkObjectType 0 "time" "description" (String "sysUptime")
        , mkObjectType 1 "time" "now"  (TimeTicks (fromIntegral t))
        ]

getSysUptime :: IO SysUptime
getSysUptime = do
    t <- flip div' 1 <$> getPOSIXTime
    return $ SysUptime $ fromIntegral t

type Full = [(OID, IORef Value)]

-- saved value
saved :: IORef Value -> IORef Value -> Full -> IO [MIB]
saved io str base = do
    x <- readIORef io
    y <- readIORef str
    return $ mkObject 3 "Fixmon" "saved" (ReadWrite (saved io str base) (saveIO base))
           : mkObjectType 0 "saved" "integer" x
           : mkObjectType 1 "saved" "string" y
           : []

otherSaved :: IORef Value -> IORef Value -> Full -> IO [MIB]
otherSaved io str base = do
    x <- readIORef io
    y <- readIORef str
    return $ mkObject 4 "Fixmon" "other" (ReadWrite (saved io str base) (saveIO base))
           : mkObjectType 0 "other" "integer" x
           : mkObjectType 1 "other" "string" y
           : []

saveIO :: Full -> [(OID, Value)] -> IO ()
saveIO _ [] = return ()
saveIO base (x:xs) = 
    case lookup (fst x) base of
         Nothing -> throwIO NotDescribedUpdate
         Just io -> atomicWriteIORef io (snd x) >> saveIO base xs

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
    s <- getSysUptime
    let st = AgentXState s (PacketID 1) (toZipper tree) sock
    -- responder tree sock sid tid pid
    listener sock (snmpAgent st)
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

type AG = StateT AgentXState IO

instance AgentM AG

listener :: Socket -> (Packet -> AG Packet) -> IO ()
listener sock f = do
    packet <- recvPacket sock
    r <- undefined
    undefined
{--
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

recvPacket :: Socket -> IO Packet
recvPacket sock = do
    h <- recv sock 20
    b <- recv sock (getBodySizeFromHeader h)
    return $ decode (h <> b)
--}

mibToRegisterPdu :: MIB -> PDU
mibToRegisterPdu m = Register Nothing (Timeout 200) (Priority 127) (RangeSubid 0) (oid m) Nothing

