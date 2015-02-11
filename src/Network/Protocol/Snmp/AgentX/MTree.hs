{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Network.Protocol.Snmp.AgentX.MTree where

import Control.Monad.State.Strict
import Control.Concurrent.MVar
import Control.Applicative hiding (empty)

import Network.Protocol.Snmp (Value(..), OID)
import Data.Map.Strict (Map)
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

data PVal = Fixed CValue
          | Read 
            { readAIO        :: forall m . (Monad m, MonadIO m) => m CValue }
          | ReadWrite 
            { readAIO        :: forall m . (Monad m, MonadIO m) => m CValue
            , commitSetAIO   :: forall m . (Monad m, MonadIO m) => Maybe Context -> Value -> m CommitError
            , testSetAIO     :: forall m . (Monad m, MonadIO m) => Maybe Context -> Value -> m TestError
            , undoSetAIO     :: forall m . (Monad m, MonadIO m) => Maybe Context -> Value -> m UndoError
            }

data MTree a = forall m . (Monad m, MonadIO m) => Node a (MTree a) (m (MTree a))
             | Leaf a (MTree a) 
             | Empty


data Move a = Next (MTree a) 
            | Level (MTree a)

type Moving a = [Move a]

data Zipper = Zipper
  { zipper        :: MTree Point
  , moving        :: Moving Point
  , toRegister    :: MVar [MIB]
  , toUnRegister  :: MVar [MIB]
  }

type Base = StateT Zipper 

empty :: MVar [MIB] -> MVar [MIB] -> Zipper 
empty = Zipper Empty [] 

insert :: MIB -> Base IO ()
insert _mib = do
    undefined

top :: Base IO ()
top = undefined

goUp :: Base IO ()
goUp = undefined

goNext :: Base IO ()
goNext = undefined

goLevel :: Base IO ()
goLevel = undefined

goBack :: Base IO ()
goBack = undefined

focus :: Base IO (OID, Point)
focus = undefined

isEmpty :: Base IO Bool
isEmpty = isEmpty' <$> getz
  where
  isEmpty' :: MTree Point -> Bool 
  isEmpty' Empty = True
  isEmpty' _     = False

isTop :: Base IO Bool
isTop = null <$> (getz :: Base IO (Moving Point))

class STZip a where
    getz :: Base IO a
    setz :: a -> Base IO ()

instance STZip (MTree Point) where
    getz = zipper <$> get
    setz x = modify $ \st -> st { zipper = x }

instance STZip (Moving Point) where
    getz = moving <$> get
    setz x = modify $ \st -> st { moving = x }

