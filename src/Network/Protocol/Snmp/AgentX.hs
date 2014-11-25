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

import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy
import Data.Binary
import Data.Monoid



fixmon :: IO ATree
fixmon = do
    io <- newIORef (Integer 0)
    io1 <- newIORef (Integer 1)
    so <- newIORef (String "")
    let b = (base [1,3,6,1,4,1,44729] "Fixmon")
    t <- generateTime
    n <- generateInterfaces
    i <- genSave io
    m <- genSaveMul io1 so
    return $ root b [n, i, m]

generateTime :: IO ATree
generateTime = do
    t <- flip div' 1 <$> getPOSIXTime
    let time = leaf 0 "time" (TimeTicks (fromIntegral t)) (ReadOnly generateTime) 
    return time 

generateInterfaces :: IO ATree
generateInterfaces = do
    nx <- getNetworkInterfaces
    let baseLeaf = leaf 1 "interfaces" Zero (ReadOnly generateInterfaces)
        xs = zip [0 .. fromIntegral $ length nx - 1] nx
        indexes = flip map xs $ \(i,_) -> leaf i "" (Integer . fromIntegral $ i) Fixed
        names = flip map xs $ \(i, o) -> leaf i "name" (String . pack . NI.name $ o) Fixed
        ipv4s = flip map xs $ \(i, o) -> leaf i "ipv4" (String . pack . show . NI.ipv4 $ o) Fixed
        ipv6s = flip map xs $ \(i, o) -> leaf i "ipv6" (String . pack . show . NI.ipv6 $ o) Fixed
        macs = flip map xs $ \(i, o) -> leaf i "mac" (String . pack . show . NI.mac $ o) Fixed
    return $ root baseLeaf $
        [ root (leaf 0 "indexes" Zero Fixed) indexes 
        , root (leaf 1 "name" Zero Fixed) names 
        , root (leaf 2 "ipv4" Zero Fixed) ipv4s 
        , root (leaf 3 "ipv6" Zero Fixed) ipv6s 
        , root (leaf 4 "mac" Zero Fixed) macs 
        ]

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

check :: IO ()
check = do
    tree <- fixmon
    sock <- socket AF_UNIX Stream 0
    connect sock (SockAddrUnix "/var/agentx/master")
    let open = Open (Timeout 200) [1,3,6,1,4,1,44729] (Description "Haskell AgentX sub-aagent")
    -- open
    sendAll sock (encode $ Packet 1 open (Flags False False False False False) (SessionID 0) (TransactionID 0) (PacketID 1))
    -- for lazy wait more big chunk then i have, and here i fix size for recv
    h <- recv sock 20
    b <- recv sock (getBodySizeFromHeader h)
    let p = decode (h <> b) :: Packet
    print p
    sid <- newIORef $ getSid p
    tid <- newIORef $ getTid p
    pid <- newIORef $ getPid p
    F.mapM_ (registerAll sock sid tid pid) $ fullOidTree tree
    where
      registerAll sock sid tid pid values = do
          s <- readIORef sid
          t <-  readIORef tid 
          p <-  atomicModifyIORef pid $ \x -> (succ x, succ x)
          sendAll sock (encode $ Packet 1 (valuesToPacket values) (Flags False False False False False) (SessionID s) (TransactionID t) (PacketID p))
          h <- recv sock 20
          b <- recv sock (getBodySizeFromHeader h)
          let request = decode (h <> b) :: Packet
          print request

      getPid (Packet _ _ _ _ _ (PacketID x)) = x
      getTid (Packet _ _ _ _ (TransactionID x) _) = x
      getSid (Packet _ _ _ (SessionID x) _ _) = x
      
    {--
    sendAll sock (encode $ Packet 1 register (Flags False False False False False) (sessionId p) (TransactionID 1) (PacketID 2))
    h <- recv sock 20
    b <- recv sock (getBodySizeFromHeader h)
    let p = decode (h <> b) :: Packet
    print p
    do
        h <- recv sock 20
        b <- recv sock (getBodySizeFromHeader h)
        let g = decode (h <> b) :: Packet
        print g
        let response = Response (sysUptime p ) NoAgentXError (Index 0) [VarBind [1,3,6,1,4,1,44729,0,0] (Integer 100)]
        sendAll sock (encode $ Packet 1 response (Flags False False False False False) (sessionId g) (TransactionID 1) (PacketID 2))
    print "end"
    --}



