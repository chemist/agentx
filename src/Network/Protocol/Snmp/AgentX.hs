module Network.Protocol.Snmp.AgentX ( 
-- * About
-- $about
  MIB
, Value(..)
, PVal(..)
, rwValue
, rsValue
, rdValue
, Update(..)
, mkObject
, mkObjectType
, CommitError(..)
, TestError(..)
, UndoError(..)
, Context
, agent
, Client
-- * Usage
-- ** Imports
-- $imports

-- ** Desribe values for monitoring
-- $values

-- ** Build dynamic tree
-- $dynTree

-- ** Describe context 
-- $context

-- ** Construct full tree
-- $fullTree

-- ** Start subagent
-- $dowork

-- ** Examples
-- $config
)
where

import Network.Protocol.Snmp 
import Network.Protocol.Snmp.AgentX.Service 
import Network.Protocol.Snmp.AgentX.MIBTree 
import Network.Protocol.Snmp.AgentX.Packet 

{- $about
Library for write extensible SNMP agents.
-}

{- $imports
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE RankNTypes #-}
> module Main where
> 
> import Network.Protocol.Snmp.AgentX 
> import qualified Data.ByteString as BS
> import Data.ByteString (ByteString)
> import Data.Fixed (div')
> import Data.Time.Clock.POSIX (getPOSIXTime)
> import Control.Applicative ((<$>))
> import Control.Monad.State
> import Data.IORef
-}


{- $values
> str :: ByteString -> PVal
> str x = rsValue (String x)
> 
> now :: PVal
> now = rdValue $  TimeTicks . flip div' 1 <$> liftIO getPOSIXTime
> 
> rws :: IORef Value -> PVal
> rws io = rwValue readV commit test undo
>   where
>     test (String x) 
>       | BS.length x < 10 = return NoTestError
>       | otherwise = return TooBig
>     test _ = return WrongType
>     commit v = do
>         writeIORef io v
>         return NoCommitError
>     undo _ = return NoUndoError
>     readV = readIORef io
> 
> rwi :: IORef Value -> PVal
> rwi io = rwValue readV commit test undo
>   where
>     test (Integer x) 
>       | x < 5 = return NoTestError
>       | otherwise = return TooBig
>     test _ = return WrongType
>     commit v = do
>         writeIORef io v
>         return NoCommitError
>     undo _ = return NoUndoError
>     readV = readIORef io
-}

{- $dynTree
> dynTree :: IORef Value -> Update
> dynTree i = Update $ do
>     Integer count <- liftIO $ readIORef i
>     return $ concatMap fun [0 .. count]
>     where
>     fun x = [ mkObject (fromIntegral x) "trees" ("tree" ++ show x) Nothing
>             , mkObjectType 0 ("tree" ++ show x) "abr" Nothing (str "abr")
>             , mkObjectType 1 ("tree" ++ show x) "abrvalg" Nothing (str "abrvalg")
>             ]
> 
-}

{- $context
> ver :: Maybe Context
> ver = Just "version"
-}

{- $fullTree
> simpleTree :: IORef Value -> IORef Value -> [MIB]
> simpleTree m i = 
>       [ mkObject 0 "Fixmon" "about" Nothing
>       , mkObjectType 0 "about" "name" Nothing $ rsValue (String "Fixmon agent")
>       , mkObjectType 1 "about" "version" Nothing $ rsValue (String "0.0.1")
>       , mkObjectType 1 "about" "version" ver $ rsValue (String "Alpha")
>       , mkObjectType 2 "about" "comment" Nothing (rws m) 
>       , mkObject 1 "Fixmon" "dyn" Nothing
>       , mkObjectType 0 "dyn" "counter" Nothing (rwi i)
>       , mkObject 1 "dyn" "trees" (Just $ dynTree i)
>       ]
> 
> tree :: IO [MIB]
> tree = do
>     m <- newIORef (String "init")
>     i <- newIORef (Integer 0)
>     return $ simpleTree m i
> 
-}
    
{- $dowork
> main :: IO ()
> main = agent "/var/agentx/master" [1,3,6,1,4,1,44729] Nothing =<< tree
> 
-}

{- $config
> SNMP server config
> > cat /etc/snmp/snmpd.conf
> > rwuser sha
> > createUser sha SHA "password" DESi
> > master agentx
> > agentXPerms 777 775
> 
> SNMP client config
> > cat /etc/snmp/snmp.conf
> > defVersion 3
> > defSecurityName sha
> > defSecurityLevel authPriv
> > defPassphrase password
> > defAuthType SHA
> > defPrivType DES
> > defContext ""
> 
> Build and start example
> > cabal install example
> > .cabal-sandbox/bin/example
> > .cabal-sandbox/bin/agentx
> > "set sid SessionID 15"
> > "R ([1,3,6,1,4,1,44729,1,1,0,1],Nothing)"
> > "R ([1,3,6,1,4,1,44729,1,1,0,0],Nothing)"
> > "R ([1,3,6,1,4,1,44729,1,0],Nothing)"
> > "R ([1,3,6,1,4,1,44729,0,2],Nothing)"
> > "R ([1,3,6,1,4,1,44729,0,1],Just (Context \"version\"))"
> > "R ([1,3,6,1,4,1,44729,0,1],Nothing)"
> > "R ([1,3,6,1,4,1,44729,0,0],Nothing)"
> 
> Get MIB tree
>  > snmpwalk -r 1 -On localhost 1.3.6.1.4.1.44729
>  > .1.3.6.1.4.1.44729.0.0 = STRING: "Fixmon agent"
>  > .1.3.6.1.4.1.44729.0.1 = STRING: "0.0.1"
>  > .1.3.6.1.4.1.44729.0.2 = STRING: "init"
>  > .1.3.6.1.4.1.44729.1.0 = INTEGER: 0
>  > .1.3.6.1.4.1.44729.1.1.0.0 = STRING: "abr"
>  > .1.3.6.1.4.1.44729.1.1.0.1 = STRING: "abrvalg"
>  
> Change MIB tree 
>  >  snmpset -r 1  -On localhost .1.3.6.1.4.1.44729.1.0 i 2
>  > .1.3.6.1.4.1.44729.1.0 = INTEGER: 2
>  >  snmpwalk -r 1 -On localhost 1.3.6.1.4.1.44729
>  > .1.3.6.1.4.1.44729.0.0 = STRING: "Fixmon agent"
>  > .1.3.6.1.4.1.44729.0.1 = STRING: "0.0.1"
>  > .1.3.6.1.4.1.44729.0.2 = STRING: "init"
>  > .1.3.6.1.4.1.44729.1.0 = INTEGER: 2
>  > .1.3.6.1.4.1.44729.1.1.0.0 = STRING: "abr"
>  > .1.3.6.1.4.1.44729.1.1.0.1 = STRING: "abrvalg"
>  > .1.3.6.1.4.1.44729.1.1.1.0 = STRING: "abr"
>  > .1.3.6.1.4.1.44729.1.1.1.1 = STRING: "abrvalg"
>  > .1.3.6.1.4.1.44729.1.1.2.0 = STRING: "abr"
>  > .1.3.6.1.4.1.44729.1.1.2.1 = STRING: "abrvalg"
> 
-}
