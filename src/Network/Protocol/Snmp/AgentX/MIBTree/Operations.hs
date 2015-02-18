{-# LANGUAGE FlexibleContexts #-}
module Network.Protocol.Snmp.AgentX.MIBTree.Operations where

import Control.Monad.IO.Class (MonadIO)
-- import Control.Monad.Trans.Class (lift)
-- import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.Packet (Context)
import Network.Protocol.Snmp.AgentX.MIBTree.Types (ZipperM)
import Network.Protocol.Snmp.AgentX.MIBTree.Zipper (Zipper)
import Network.Protocol.Snmp.AgentX.MIBTree.MTree (MTree)
import qualified Network.Protocol.Snmp.AgentX.MIBTree.Zipper as Z
import qualified Network.Protocol.Snmp.AgentX.MIBTree.Types as Z
import Data.Label.Monadic (modify, gets)
import Control.Applicative ((<$>))
-- import Control.Concurrent.MVar
import Data.Maybe (fromMaybe)


goNext ::(Monad m, MonadIO m, Functor m) =>  ZipperM m a ()
goNext = modify Z.zipper $ change Z.goNext  

change :: (a -> Maybe a) -> a -> a
change f x = fromMaybe x (f x)

move :: (Monad m, MonadIO m, Functor m) => (Zipper MTree a -> Maybe (Zipper MTree a)) -> ZipperM m a ()
move x = modify Z.zipper $ change x

goLevel :: (Monad m, MonadIO m, Functor m) => ZipperM m a ()
goLevel = move Z.goLevel

goBack :: (Monad m, MonadIO m, Functor m) => ZipperM m a ()
goBack = move Z.goBack


top :: (Monad m, MonadIO m, Functor m) => ZipperM m a ()
top = modify Z.zipper Z.top 


goUp :: (Monad m, MonadIO m, Functor m) => ZipperM m a ()
goUp = move Z.goUp


cursor :: (Monad m, MonadIO m, Functor m) => ZipperM m a (Maybe (Integer, Maybe Context))
cursor = Z.cursor <$> gets Z.zipper 
{--
focus :: (Monad m, MonadIO m, Functor m, Show a) => ZipperM m a a
focus = gets Z.zipper
--}
             

