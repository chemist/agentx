{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString (ByteString)
import Control.Exception (bracket)

import Network.Snmp.Client
import Network.Protocol.Snmp
import Control.Monad

main :: IO ()
main = bracket (client conf)
               close
               requests

conf :: Config
conf = (initial Version2) { hostname = "127.0.0.1"
                          , community = Community "public"
                          }

dev :: ByteString
dev = "1.3.6.1.4.1.44729.2.1.7"

requests :: Client -> IO ()
requests snmp = forever $ do
    putStr . show =<< get snmp [oidFromBS dev]
