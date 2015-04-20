{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Network.Protocol.Snmp.AgentX 
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Applicative ((<$>))
import Control.Monad.State
import Data.IORef



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
    return $ concatMap fun [0 .. count]
    where
    fun x = [ mkObject (fromIntegral x) "trees" ("tree" ++ show x) Nothing
            , mkObjectType 0 ("tree" ++ show x) "abr" Nothing (str "abr")
            , mkObjectType 1 ("tree" ++ show x) "abrvalg" Nothing (str "abrvalg")
            ]

ver :: Maybe Context
ver = Just "version"

simpleTree :: IORef Value -> IORef Value -> [MIB]
simpleTree m i = 
      [ mkObject 0 "Fixmon" "about" Nothing
      , mkObjectType 0 "about" "name" Nothing $ rsValue (String "Fixmon agent")
      , mkObjectType 1 "about" "version" Nothing $ rsValue (String "0.0.1")
      , mkObjectType 1 "about" "version" ver $ rsValue (String "Alpha")
      , mkObjectType 2 "about" "comment" Nothing (rws m) 
      , mkObject 1 "Fixmon" "dyn" Nothing
      , mkObjectType 0 "dyn" "counter" Nothing (rwi i)
      , mkObject 1 "dyn" "trees" (Just $ dynTree i)
      ]

tree :: IO [MIB]
tree = do
    m <- newIORef (String "init")
    i <- newIORef (Integer 0)
    return $ simpleTree m i
    
main :: IO ()
main = agent "/var/agentx/master" [1,3,6,1,4,1,44729] Nothing =<< tree

