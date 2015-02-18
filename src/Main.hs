{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Network.Protocol.Snmp.AgentX (Value(..))
import Network.Protocol.Snmp.AgentX.MIBTree.Types 
import Network.Protocol.Snmp.AgentX.MIBTree.MIB 
import Network.Info
import qualified Network.Info as NI
import Data.ByteString.Char8 (pack)
import Data.Monoid ((<>))
-- import Network.Protocol.Snmp.AgentX.MIBTree.Operations  
-- import qualified Data.Map.Strict as Map
-- import Control.Concurrent.MVar
-- import Control.Monad.State.Strict

pv1 :: PVal IO
pv1 = rsValue (String "hello")

pv2 :: PVal IO
pv2 = rsValue (Integer 1)

subTree :: UpdateM IO 
subTree = Update (return ls)
  where
    ls :: [MIBM IO]
    ls =
        [ mkObject 3 "dyn" "tree" Nothing
        , mkObjectType 0 "tree" "about" Nothing pv1
        , mkObjectType 1 "tree" "name" Nothing pv1
        ]

interfaces :: UpdateM IO 
interfaces = Update ifaces
    where 
    ifaces = do
        nx <- getNetworkInterfaces
        let xs = zip [0 .. fromIntegral $ length nx - 1] nx
            indexes, names, ipv4s, ipv6s, macs :: [MIBM IO]
            indexes = flip map xs $ \(i,_) -> mkObjectType i "indexes" "index" Nothing (rsValue . Integer . fromIntegral $ i) 
            names = flip map xs $ \(i, o) -> mkObjectType i "names" "name" Nothing (rsValue . String . pack . NI.name $ o) 
            ipv4s = flip map xs $ \(i, o) -> mkObjectType i "ipv4s" "ipv4" Nothing (rsValue . String . pack . show . NI.ipv4 $ o) 
            ipv6s = flip map xs $ \(i, o) -> mkObjectType i "ipv6s" "ipv6" Nothing (rsValue . String . pack . show . NI.ipv6 $ o) 
            macs = flip map xs $ \(i, o) -> mkObjectType i "macs" "mac" Nothing (rsValue . String . pack . show . NI.mac $ o) 
        return $ 
            mkObject 4 "net" "interfaces" Nothing :
              (mkObject 0 "interfaces" "indexes" Nothing : indexes) <>
              (mkObject 1 "interfaces" "names"   Nothing : names)   <>
              (mkObject 2 "interfaces" "ipv4s"   Nothing : ipv4s)   <>
              (mkObject 3 "interfaces" "ipv6s"   Nothing : ipv6s)   <>
              (mkObject 4 "interfaces" "macs"    (Just subTree) : macs) 

simpleTree :: [MIBM IO]
simpleTree = 
      [ mkObject 0 "Fixmon" "about" Nothing
      , mkObjectType 0 "about" "name" Nothing pv1 
      , mkObjectType 1 "about" "version" Nothing pv1
      , mkObjectType 2 "about" "contexted" Nothing pv2
      , mkObject 1 "Fixmon" "dyn" Nothing
      , mkObjectType 0 "dyn" "name" Nothing pv1 
      , mkObjectType 1 "dyn" "version" Nothing pv1
      , mkObjectType 2 "dyn" "contexted" Nothing pv2
      , mkObject 3 "dyn" "tree" (Just subTree)
      , mkObject 4 "dyn" "net" (Just interfaces)
      ]

main :: IO ()
main = undefined



{--
import Network.Protocol.Snmp.AgentX
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
    nodes <- newIORef $ Integer 1
    dynT <- dynamicTree nodes
    return $ fromList $ 
      [ mkModule [1,3,6,1,4,1,44729] "enterprise" "Fixmon"
      , mkObject 0 "Fixmon" "about" EmptyTree
      , mkObjectType 0 "about" "name" (defaultContext (String "fixmon snmp agent")) (updateName agentName) 
      , mkObjectType 1 "about" "version" (defaultContext (Integer 1)) Fixed
      , mkObjectType 2 "about" "contexted" contextedValue Fixed
      ] <> interfaces' <> dynT <> time 

contextedValue :: ContextedValue
contextedValue = Map.fromList [ ("context1", String "context1")
                              , ("context2", String "context2")
                              , ("", String "defalut")
                              ]

updateName :: IORef Value -> Update
updateName agentName = ReadWrite (defaultContext <$> readIORef agentName) 
                                 (commitSet agentName) 
                                 (testSet agentName)
                                 (undoSet agentName)

commitSet :: IORef Value -> Maybe Context -> Value -> IO CommitError
commitSet ioref _ v = do
    writeIORef ioref v
    return NoCommitError


testSet :: IORef Value -> Maybe Context -> Value -> IO TestError
testSet _ _ (String _) = return NoTestError
testSet _ _ _ = return WrongType

testSetInt :: IORef Value -> Maybe Context -> Value -> IO TestError
testSetInt _ _ (Integer _) = return NoTestError
testSetInt _ _ _ = return WrongType

undoSet :: IORef Value -> Maybe Context -> Value -> IO UndoError
undoSet _ _ _ = return NoUndoError

dynamicTree :: IORef Value -> IO [MIB]
dynamicTree nodes = do
    Integer i <- readIORef nodes
    return $  mkObject 1 "Fixmon" "dynamic" (DynTree (dynamicTree nodes )) : map addNode [0 .. fromIntegral i]
    where
    addNode :: Integer -> MIB
    addNode 0 = mkObjectType 0 "dynamic" "counter" (defaultContext (Integer 0)) updateCounter
    addNode i = mkObjectType i "dynamic" (show i) (defaultContext (Integer $ fromIntegral i)) Fixed
    updateCounter = ReadWrite (defaultContext <$> readIORef nodes)
                              (commitSet nodes)
                              (testSetInt nodes)
                              (undoSet nodes)



time :: [MIB]
time = 
    [ mkObject 3 "Fixmon" "time" EmptyTree
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
        mkObject 2 "Fixmon" "interfaces" (DynTree interfaces) :
          (mkObject 0 "interfaces" "indexes" EmptyTree : indexes) <>
          (mkObject 1 "interfaces" "names"   EmptyTree : names )  <>
          (mkObject 2 "interfaces" "ipv4s"   EmptyTree : ipv4s )  <>
          (mkObject 3 "interfaces" "ipv6s"   EmptyTree : ipv6s )  <>
          (mkObject 4 "interfaces" "macs"    EmptyTree : macs )

main :: IO ()
main = agent "/var/agentx/master" =<< fixmon

--}
