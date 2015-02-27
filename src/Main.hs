{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Network.Protocol.Snmp.AgentX (Value(..))
import Network.Protocol.Snmp.AgentX.MIBTree
import Network.Protocol.Snmp.AgentX.Service
import Network.Info
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.ByteString (ByteString)
import qualified Network.Info as NI
import Data.ByteString.Char8 (pack)
import Data.Monoid ((<>))
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Applicative ((<$>))


pv1 :: PVal
pv1 = rsValue (String "hello")

pv2 :: PVal
pv2 = rsValue (Integer 1)

str :: ByteString -> PVal
str x = rsValue (String x)

now :: PVal
now = rdValue $  TimeTicks . flip div' 1 <$> liftIO getPOSIXTime

subTree :: Update 
subTree = Update (return ls)
  where
    ls =
        [ mkObjectType 0 "tree" "about" Nothing (str "subTree")
        , mkObjectType 1 "tree" "name" Nothing pv1
        , mkObject 2 "tree" "sub" (Just subTree1)
        , mkObject 3 "tree" "sub" (Just subTree2)
        ]

subTree1 :: Update 
subTree1 = Update (return ls)
  where
    ls = [ mkObjectType 0 "tree" "about" Nothing (rsValue (String "subTree1"))
         , mkObjectType 1 "tree" "name" Nothing now
         , mkObject 2 "tree" "sub" (Just subTree2)
         ]

subTree2 :: Update 
subTree2 = Update (return ls)
  where
    ls = [ mkObjectType 0 "tree" "about" Nothing (rsValue (String "subTree2"))
         , mkObjectType 1 "tree" "name" Nothing pv1
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

simpleTree :: [MIB]
simpleTree = 
      [ mkObject 0 "Fixmon" "about" Nothing
      , mkObjectType 0 "about" "name" Nothing $ rsValue (String "Fixmon agent")
      , mkObjectType 1 "about" "version" Nothing $ rsValue (String "0.0.1")
      , mkObjectType 1 "about" "version" (Just "version") $ rsValue (String "Alpha")
      , mkObject 1 "Fixmon" "dyn" Nothing
      , mkObjectType 0 "dyn" "name" Nothing pv1 
      , mkObjectType 1 "dyn" "version" Nothing pv1
      , mkObjectType 2 "dyn" "contexted" Nothing pv2
      , mkObject 3 "dyn" "tree" (Just subTree)
      , mkObject 4 "dyn" "net" (Just subTree1)
      , mkObject 2 "Fixmon" "interfaces" (Just interfaces)
      ]





main :: IO ()
main = agent "/var/agentx/master" [1,3,6,1,4,1,44729] simpleTree

