{-# LANGUAGE FlexibleContexts #-}
module Network.Protocol.Snmp.AgentX.MIBTree.Operations where

import Control.Monad.IO.Class (MonadIO)
-- import Control.Monad.Trans.Class (lift)
-- import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.MIBTree.Types (ZipperM, Zipper)
import qualified Network.Protocol.Snmp.AgentX.MIBTree.Types as Z
import Data.Label.Monadic (modify, gets, puts)
import Control.Applicative ((<$>))
-- import Control.Concurrent.MVar
import Data.Maybe (fromMaybe)


goNext ::(Monad m, MonadIO m, Functor m) =>  ZipperM m a ()
goNext = modify Z.zipper $ change Z.goNext  

change :: (a -> Maybe a) -> a -> a
change f x = fromMaybe x (f x)

move :: (Monad m, MonadIO m, Functor m) => (Zipper a -> Maybe (Zipper a)) -> ZipperM m a ()
move x = modify Z.zipper $ change x

goLevel :: (Monad m, MonadIO m, Functor m) => ZipperM m a ()
goLevel = move Z.goLevel

goBack :: (Monad m, MonadIO m, Functor m) => ZipperM m a ()
goBack = move Z.goBack


top :: (Monad m, MonadIO m, Functor m) => ZipperM m a ()
top = modify Z.zipper Z.top >> puts Z.oid []


goUp :: (Monad m, MonadIO m, Functor m) => ZipperM m a ()
goUp = move Z.goUp


cursor :: (Monad m, MonadIO m, Functor m) => ZipperM m a (Maybe Integer)
cursor = Z.cursor <$> gets Z.zipper 
{--
focus :: (Monad m, MonadIO m, Functor m, Show a) => ZipperM m a a
focus = gets Z.zipper
--}
             

