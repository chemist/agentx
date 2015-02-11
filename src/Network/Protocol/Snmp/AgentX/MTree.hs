{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Network.Protocol.Snmp.AgentX.MTree where

import Control.Monad.State.Strict hiding (gets, modify)
import Control.Concurrent.MVar
import Control.Applicative hiding (empty)
import Data.Label (mkLabel)
import Data.Label.Monadic (gets, puts, modify)
-- import qualified Data.Label as DL
-- import qualified Data.Label.Monadic as DLM
-- import Control.Category ((.))
import Prelude hiding ((.))

import Network.Protocol.Snmp (Value(..), OID)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.Protocol.Snmp.AgentX.Packet (Context, CommitError, TestError, UndoError)

data MIB = Object OID Update
         | ObjectType OID PVal 

type Update = forall m . (Monad m, MonadIO m) =>  m [MIB]

mkStaticObject :: OID -> MIB 
mkStaticObject oid = Object oid (return [])

mkDynamicObject :: OID -> Update -> MIB 
mkDynamicObject oid fun = Object oid fun

mkObjectType :: OID -> PVal -> MIB 
mkObjectType = ObjectType 

data Point = Root OID Integer
           | Dir Integer
           | Point Integer PVal


type CValue = Map Context Value

data PVal = Read 
            { readAIO        :: forall m . (Monad m, MonadIO m) => m CValue }
          | ReadWrite 
            { readAIO        :: forall m . (Monad m, MonadIO m) => m CValue
            , commitSetAIO   :: forall m . (Monad m, MonadIO m) => Maybe Context -> Value -> m CommitError
            , testSetAIO     :: forall m . (Monad m, MonadIO m) => Maybe Context -> Value -> m TestError
            , undoSetAIO     :: forall m . (Monad m, MonadIO m) => Maybe Context -> Value -> m UndoError
            }

p :: (Monad m, MonadIO m) => m CValue
p = do
    liftIO $ print "io"
    return Map.empty

pv :: PVal
pv = Read p


data MTree a = Node a (MTree a) (IO (MTree a))
             | Leaf a (MTree a) 
             | Empty


data Move a = Next (MTree a) 
            | Level (IO (MTree a))

type Moving a = [Move a]

data Zipper = Zipper
  { _zipper        :: MTree Point
  , _moving        :: Moving Point
  , _oid           :: OID
  , _reg           :: MVar [MIB]
  , _unreg         :: MVar [MIB]
  }

mkLabel ''Zipper

type Base = StateT Zipper 

empty :: MVar [MIB] -> MVar [MIB] -> Zipper 
empty = Zipper Empty [] []

insert :: (Monad m, MonadIO m, Functor m) => MIB -> Base m ()
insert _mib = do
    undefined

top :: (Monad m, MonadIO m, Functor m) => Base m Bool
top = undefined

goUp :: (Monad m, MonadIO m, Functor m) => Base m Bool
goUp = undefined

goNext :: (Monad m, MonadIO m, Functor m) => Base m Bool
goNext = do
   z <-  gets zipper
   case z of
        Empty -> return False
        Leaf _ Empty -> return False
        Node _ Empty _ -> return False
        Leaf a next -> do
            puts zipper next
            addMove (Next $ Leaf a Empty)
            return True
        Node a next level -> do
            puts zipper next
            addMove (Next $ Node a Empty level)
            return True

addMove :: (Monad m, MonadIO m, Functor m) => Move Point -> Base m ()
addMove x = modify moving $ \xs -> x : xs

goLevel :: (Monad m, MonadIO m, Functor m) => Base m Bool
goLevel = do
    z <- gets zipper
    case z of
         Empty -> return False
         Leaf{} -> return False
         Node _ _next mlink -> do
             link <-  liftIO $ mlink
             case link of
                  Empty -> return False
                  _ -> do
                      puts zipper link
                      addMove $ Level mlink
                      return True




goBack :: (Monad m, MonadIO m, Functor m) => Base m Bool
goBack = undefined

focus :: (Monad m, MonadIO m, Functor m) => Base m (OID, Point)
focus = undefined

isEmpty :: (Monad m, MonadIO m, Functor m) => Base m Bool
isEmpty = isEmpty' <$> gets zipper
  where
  isEmpty' :: MTree Point -> Bool 
  isEmpty' Empty = True
  isEmpty' _     = False

isTop :: (Monad m, MonadIO m, Functor m) => Base m Bool
isTop = null <$> gets moving


