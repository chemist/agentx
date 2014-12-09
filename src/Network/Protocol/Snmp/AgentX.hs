{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.Snmp.AgentX where

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Monads (agent)
import Network.Protocol.Snmp.AgentX.MIBTree
import Data.Tree
import Network.Info
import qualified Network.Info as NI
import Data.ByteString.Char8 (pack)
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Applicative
import Data.IORef
import Control.Exception (catch, throwIO)
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
    return $ fromListWithBase "enterprise" [1,3,6,1,4,1] $ 
      [ mkModule 44729 "enterprise" "Fixmon"
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

main :: IO ()
main = agent "/var/agentx/master" =<< fixmon
