{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Network.Protocol.Snmp (Value(..))
import Network.Protocol.Snmp.AgentX.Service (agent)
import Network.Protocol.Snmp.AgentX.MIBTree
import Network.Info
import qualified Network.Info as NI
import Data.ByteString.Char8 (pack)
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Applicative
import Data.Monoid

fixmon :: IO (MIBTree MIB)
fixmon = do
    interfaces' <- interfaces
    return $ fromListWithBase "enterprise" [1,3,6,1,4,1] $ 
      [ mkModule 44729 "enterprise" "Fixmon"
      , mkObject 0 "Fixmon" "about" Nothing
      , mkObjectType 0 "about" "name" (String "fixmon snmp agent") Fixed
      , mkObjectType 1 "about" "version" (Integer 1) Fixed
      ] <> interfaces' <> time

time :: [MIB]
time = 
    [ mkObject 1 "Fixmon" "time" Nothing
    , mkObjectType 0 "time" "description" (String "sysUptime") Fixed
    , mkObjectType 1 "time" "now"  (TimeTicks 0) (Read fun)
    ]
    where
    fun :: IO Value
    fun = TimeTicks . flip div' 1 <$> getPOSIXTime

interfaces :: IO [MIB]
interfaces = do
    nx <- getNetworkInterfaces
    let xs = zip [0 .. fromIntegral $ length nx - 1] nx
        indexes = flip map xs $ \(i,_) -> mkObjectType i "indexes" "index" (Integer . fromIntegral $ i) Fixed
        names = flip map xs $ \(i, o) -> mkObjectType i "names" "name" (String . pack . NI.name $ o) Fixed
        ipv4s = flip map xs $ \(i, o) -> mkObjectType i "ipv4s" "ipv4" (String . pack . show . NI.ipv4 $ o) Fixed
        ipv6s = flip map xs $ \(i, o) -> mkObjectType i "ipv6s" "ipv6" (String . pack . show . NI.ipv6 $ o) Fixed
        macs = flip map xs $ \(i, o) -> mkObjectType i "macs" "mac" (String . pack . show . NI.mac $ o) Fixed
    return $ 
        mkObject 2 "Fixmon" "interfaces" (Just $ UTree interfaces) :
          (mkObject 0 "interfaces" "indexes" Nothing : indexes) <>
          (mkObject 1 "interfaces" "names"   Nothing : names )  <>
          (mkObject 2 "interfaces" "ipv4s"   Nothing : ipv4s )  <>
          (mkObject 3 "interfaces" "ipv6s"   Nothing : ipv6s )  <>
          (mkObject 4 "interfaces" "macs"    Nothing : macs )

main :: IO ()
main = agent "/var/agentx/master" =<< fixmon
