{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.Protocol.Snmp.AgentX.Monads where

import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy
import Data.Binary
import Data.Monoid
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Types
import Debug.Trace

type Reaction m = Packet -> m Packet


data AgentXState m = AgentXState
  { sysuptime :: SysUptime
  , packetCounter :: PacketID
  , name :: ByteString
  , sock :: Socket
  }

newtype AgentT s m a = AgentT
  { runAgentT :: s -> Packet -> m (Either Packet a, s) }

instance Functor m =>  Functor (AgentT s m) where
    fmap f (AgentT act) = AgentT $ \st0 req ->
        go `fmap` act st0 req
        where
          go (eaf, st) = case eaf of
                              Left resp -> (Left resp, st)
                              Right result -> (Right $ f result, st)

instance Monad m =>  Monad (AgentT s m) where
    return a = AgentT $ \st _ -> return $ (Right a, st)
    (AgentT act) >>= fun = AgentT $ \st0 req -> do
        (eres, st) <-  act st0 req
        case eres of
             Left resp -> return (Left resp, st)
             Right result -> do
                 let (AgentT fres) = fun result
                 fres st req

instance (Monad m, Functor m) =>  Applicative (AgentT s m) where
    pure = return
    (<*>) = ap

instance (Functor m, Monad m) => Alternative (AgentT s m) where
      empty = respond notFound
      (<|>) = (>>)

instance MonadTrans (AgentT s) where
    lift act = AgentT $ \st _ -> act >>= \r -> return (Right r, st)

instance MonadIO m => MonadIO (AgentT s m) where
    liftIO = lift . liftIO

instance Monad m => MonadReader Packet (AgentT s m) where
    ask = AgentT $ \st req -> return (Right req, st)
    local f (AgentT act) = AgentT $ \st req -> act st (f req)

request :: Monad m => AgentT s m Packet
request = ask

hoistEither :: Monad m => Either Packet a -> AgentT s m a
hoistEither eith = AgentT $ \st _ -> return (eith, st)

respond :: Monad m =>  Packet -> AgentT s m a
respond resp = hoistEither $ Left resp


notFound = error "notFound"

agentTReaction :: Monad m => s -> AgentT s m a -> Reaction m
agentTReaction s agent req =
      runAgentT agent s req >>=
              either return (const $ return notFound) . fst

fromReaction :: (MonadIO m, Monad m) => (Packet -> m Packet) -> AgentT s m ()
fromReaction app = do
    req <-  request
    liftIO $ print req
    resp <- lift $ app req
    respond resp

defFlags = Flags False False False False False
pdu = Ping Nothing

fun1 (Packet v pdu defFlags (SessionID 1) (TransactionID 1) (PacketID 1))
  | v == 1 = return $ (Packet 1 pdu defFlags (SessionID 1) (TransactionID 1) (PacketID 1))
  | otherwise = fail "not found"

fun2 (Packet v pdu defFlags (SessionID 1) (TransactionID 1) (PacketID 1)) 
  | v == 2 = return $ (Packet 2 pdu defFlags (SessionID 1) (TransactionID 1) (PacketID 1))
  | otherwise = fail "not found"

fun3 (Packet v pdu defFlags (SessionID 1) (TransactionID 1) (PacketID 1)) 
  | v == 3 = return $ (Packet 3 pdu defFlags (SessionID 1) (TransactionID 1) (PacketID 1))
  | otherwise = fail "not found"

getV (Packet v _ _ _ _ _) = v

routeVersion :: Monad m => Word8 -> AgentT s m a -> AgentT s m ()
routeVersion v x = do
    r <-  ask
    if v == (getV r) then x >> return () else return ()



allFun :: Reaction IO
allFun = agentTReaction () $ do
    routeVersion 1 $ fromReaction fun1
    routeVersion 2 $ fromReaction fun2
    routeVersion 3 $ fromReaction fun3


