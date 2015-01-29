{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Network.Protocol.Snmp (Value(..))
import Network.Protocol.Snmp.AgentX.Service (agent)
import Network.Protocol.Snmp.AgentX.MIBTree
import Network.Protocol.Snmp.AgentX.Protocol (RError(..))
import Network.Info
import qualified Network.Info as NI
import Data.ByteString.Char8 (pack)
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Applicative
import Data.Monoid
import Data.IORef
import qualified Data.Map.Strict as Map

fixmon :: IO MIBTree 
fixmon = do
    interfaces' <- interfaces
    agentName <- newIORef $ String "fixmon snmp agent"
    return $ fromList $ 
      [ mkModule [1,3,6,1,4,1,44729] "enterprise" "Fixmon"
      , mkObject 0 "Fixmon" "about" Nothing
      , mkObjectType 0 "about" "name" (defaultContext (String "fixmon snmp agent")) (updateName agentName) 
      , mkObjectType 1 "about" "version" (defaultContext (Integer 1)) Fixed
      , mkObjectType 2 "about" "contexted" contextedValue Fixed
--      , mkObjectType 2 "about" "contexted" (MapContext (Map.fromList [("one", Integer 1), ("two", Integer 2)])) Fixed
      ] <> interfaces' <> time

contextedValue :: ContextedValue
contextedValue = Map.fromList [ ("111", String "context1")
                              , ("context2", String "context2")
                              , ("", String "defalut")
                              ]

updateName :: IORef Value -> Update
updateName agentName = ReadWrite (defaultContext <$> readIORef agentName) 
                                 (const $ writeIORef agentName)
                                 (const $ checkType agentName)

checkType :: IORef Value -> Value -> IO RError
checkType _ (String _) = return NoAgentXError
checkType _ _ = return WrongType



time :: [MIB]
time = 
    [ mkObject 1 "Fixmon" "time" Nothing
    , mkObjectType 0 "time" "description" (defaultContext (String "sysUptime")) Fixed
    , mkObjectType 1 "time" "now"  (defaultContext (TimeTicks 0)) (Read fun)
    ]
    where
    fun :: IO ContextedValue
    fun = defaultContext . TimeTicks . flip div' 1 <$> getPOSIXTime

interfaces :: IO [MIB]
interfaces = do
    nx <- getNetworkInterfaces
    let xs = zip [0 .. fromIntegral $ length nx - 1] nx
        indexes = flip map xs $ \(i,_) -> mkObjectType i "indexes" "index" (defaultContext . Integer . fromIntegral $ i) Fixed
        names = flip map xs $ \(i, o) -> mkObjectType i "names" "name" (defaultContext . String . pack . NI.name $ o) Fixed
        ipv4s = flip map xs $ \(i, o) -> mkObjectType i "ipv4s" "ipv4" (defaultContext . String . pack . show . NI.ipv4 $ o) Fixed
        ipv6s = flip map xs $ \(i, o) -> mkObjectType i "ipv6s" "ipv6" (defaultContext . String . pack . show . NI.ipv6 $ o) Fixed
        macs = flip map xs $ \(i, o) -> mkObjectType i "macs" "mac" (defaultContext . String . pack . show . NI.mac $ o) Fixed
    return $ 
        mkObject 2 "Fixmon" "interfaces" (Just $ UTree interfaces) :
          (mkObject 0 "interfaces" "indexes" Nothing : indexes) <>
          (mkObject 1 "interfaces" "names"   Nothing : names )  <>
          (mkObject 2 "interfaces" "ipv4s"   Nothing : ipv4s )  <>
          (mkObject 3 "interfaces" "ipv6s"   Nothing : ipv6s )  <>
          (mkObject 4 "interfaces" "macs"    Nothing : macs )

main :: IO ()
main = agent "/var/agentx/master" =<< fixmon

