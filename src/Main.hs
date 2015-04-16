{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Network.Protocol.Snmp.AgentX 
import Network.Info
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Network.Info as NI
import Data.ByteString.Char8 (pack)
import Data.Monoid ((<>))
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Applicative ((<$>))
import Control.Monad.State
import Data.IORef
import GHC.Int (Int32)



str :: ByteString -> PVal
str x = rsValue (String x)

now :: PVal
now = rdValue $  TimeTicks . flip div' 1 <$> liftIO getPOSIXTime

rws :: IORef Value -> PVal
rws io = rwValue readV commit test undo
  where
    test (String x) 
      | BS.length x < 10 = return NoTestError
      | otherwise = return TooBig
    test _ = return WrongType
    commit v = do
        writeIORef io v
        return NoCommitError
    undo _ = return NoUndoError
    readV = readIORef io

rwi :: IORef Value -> PVal
rwi io = rwValue readV commit test undo
  where
    test (Integer x) 
      | x < 5 = return NoTestError
      | otherwise = return TooBig
    test _ = return WrongType
    commit v = do
        writeIORef io v
        return NoCommitError
    undo _ = return NoUndoError
    readV = readIORef io

dynTree :: IORef Value -> Update
dynTree i = Update $ do
    Integer count <- liftIO $ readIORef i
    let first = mkObjectType 0 "dyn" "count" Nothing (rwi i)
    return $ first : map (\x -> mkObject (fromIntegral x + 1) "dyn" ("count" ++ show (x + 1)) (Just (subTree x))) [0 .. count] 

subTree :: Int32 -> Update 
subTree i = Update (return ls)
  where
    ls =
        [ mkObjectType 0 ("count" ++ show (i + 1)) "first" Nothing (str "first")
        , mkObjectType 1 ("count" ++ show (i + 1)) "second" Nothing (str "second")
        ]

interfaces :: Update 
interfaces = Update ifaces
    where 
    ifaces :: (Monad m, MonadIO m, Functor m) => m [MIB]
    ifaces = do
        nx <- liftIO $ getNetworkInterfaces
        let xs = zip [0 .. fromIntegral $ length nx - 1] nx
            indexes = flip map xs $ \(i,_) -> mkObjectType i "indexes" "index" Nothing (rsValue . Integer . fromIntegral $ i) 
            names = flip map xs $ \(i, o) -> mkObjectType i "names" "name" Nothing (rsValue . String . pack . NI.name $ o) 
            ipv4s = flip map xs $ \(i, o) -> mkObjectType i "ipv4s" "ipv4" Nothing (rsValue . String . pack . show . NI.ipv4 $ o) 
            ipv6s = flip map xs $ \(i, o) -> mkObjectType i "ipv6s" "ipv6" Nothing (rsValue . String . pack . show . NI.ipv6 $ o) 
            macs = flip map xs $ \(i, o) -> mkObjectType i "macs" "mac" Nothing (rsValue . String . pack . show . NI.mac $ o) 
        return $ 
              (mkObject 0 "interfaces" "indexes" Nothing : indexes) <>
              (mkObject 1 "interfaces" "names"   Nothing : names)   <>
              (mkObject 2 "interfaces" "ipv4s"   Nothing : ipv4s)   <>
              (mkObject 3 "interfaces" "ipv6s"   Nothing : ipv6s)   <>
              (mkObject 4 "interfaces" "macs"    Nothing : macs) 

ver :: Maybe Context
ver = Just "version"

simpleTree :: IORef Value -> IORef Value -> [MIB]
simpleTree m i = 
      [ mkObject 0 "Fixmon" "about" Nothing
      , mkObjectType 0 "about" "name" Nothing $ rsValue (String "Fixmon agent")
      , mkObjectType 1 "about" "version" Nothing $ rsValue (String "0.0.1")
      , mkObjectType 1 "about" "version" ver $ rsValue (String "Alpha")
      , mkObjectType 2 "about" "comment" Nothing (rws m) 
      , mkObject 1 "Fixmon" "dyn" (Just (dynTree i))
      , mkObject 2 "Fixmon" "interfaces" (Just interfaces)
      ]

main :: IO ()
main = do
    m <- newIORef (String "init")
    i <- newIORef (Integer 0)
    agent "/var/agentx/master" [1,3,6,1,4,1,44729] (simpleTree m i)

