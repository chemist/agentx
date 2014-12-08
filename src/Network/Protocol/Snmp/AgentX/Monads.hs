{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Network.Protocol.Snmp.AgentX.Monads where

import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy
import Data.Binary hiding (get, put)
import Data.Monoid
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Data.Typeable
import Data.Either

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Types hiding (getValue)
import Network.Protocol.Snmp.AgentX.MIBTree
import Debug.Trace

data AgentXState = AgentXState
  { sysuptime :: SysUptime
  , packetCounter :: PacketID
  , mibs :: Zipper MIB
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

instance (Monad m, MonadState AgentXState m) => MonadState AgentXState (AgentT s m) where
    get = AgentT $ \st _ -> do
        x <- get
        return (Right x, st)
    put x = AgentT $ \st _ -> do
        put x
        return (Right (), st)

request :: Monad m => AgentT s m Packet
request = ask

hoistEither :: Monad m => Either Packet a -> AgentT s m a
hoistEither eith = AgentT $ \st _ -> return (eith, st)

respond :: Monad m =>  Packet -> AgentT s m a
respond resp = hoistEither $ Left resp

class (Monad m, MonadState AgentXState m, Functor m, MonadIO m) => AgentM m


notFound = error "notFound"

{--
readIncoming :: AgentM m => AgentT s m ()
readIncoming = do
    s <- sock <$> get
    p <- liftIO $ recvPacket s
    respond p
    route
    --}

recvPacket :: Socket -> IO Packet
recvPacket sock = do
    h <- recv sock 20
    b <- recv sock (getBodySizeFromHeader h)
    return $ decode (h <> b)


route :: AgentM m => AgentT s m ()
route = do
    (Packet v pdu flags sid tid pid) <- request
    liftIO $ print pdu
    case pdu of
         Get _ oids -> getHandler oids 
         _ -> undefined

getHandler :: AgentM m => [OID] -> AgentT s m ()
getHandler oids = do
    liftIO $ print "get response handler"
    liftIO $ print oids
    pdu <- makePdu =<< getHandler' oids
    (Packet v _ flags sid tid pid) <- request
    respond $ Packet v pdu flags sid tid pid
    where
        getHandler' :: AgentM m => [OID] -> AgentT s m [Either MIBException MIB]
        getHandler' [] = return []
        getHandler' (x:xs) = do
            z <- mibs <$> get
            (:) <$> (liftIO $ evalStateT (findR x) z) <*> getHandler' xs

sendPacket :: AgentM m => AgentT s m ()
sendPacket = do
    liftIO $ print "send response"
    sock <- sock <$> get
    bs <- encode <$> request
    liftIO $ sendAll sock bs

makePdu :: AgentM m => [Either MIBException MIB] -> AgentT s m PDU
makePdu xs = do
  let ls = map fun $ zip xs [1 .. ] 
  now <- sysuptime <$> get
  return $ makePdu' now $ case lefts ls of
                               [] -> Right $ rights ls
                               (x:_) -> Left x
  where 
    fun (Right x, _) = Right x
    fun (Left _, n)  = Left n
    makePdu' now (Left x) = Response now ProcessingError (Index x) []
    makePdu' now (Right x) = Response now NoAgentXError (Index 0) $ map mibToVarBind x

mibToVarBind :: MIB -> VarBind
mibToVarBind y = VarBind (oid y) (getValue y)

agentTReaction :: Monad m => s -> AgentT s m a -> Packet -> m Packet
agentTReaction s agent req =
  runAgentT agent s req >>=
    either return (const $ return notFound) . fst

snmpAgent :: AgentM m => AgentXState -> Packet -> m Packet
snmpAgent s = agentTReaction s route
