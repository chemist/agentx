{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.Protocol.Snmp.AgentX.MIBTree.Types where

import Control.Monad.State.Strict hiding (gets, modify)
-- import Control.Concurrent.MVar

import Data.Map.Strict (Map)
import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Packet (Context, CommitError, TestError, UndoError)

type CValue = Map Context Value

data PVal m = Read 
            { readAIO        :: m CValue }
          | ReadWrite 
            { readAIO        :: m CValue
            , commitSetAIO   :: Maybe Context -> Value -> m CommitError
            , testSetAIO     :: Maybe Context -> Value -> m TestError
            , undoSetAIO     :: Maybe Context -> Value -> m UndoError
            }

data MTree a = Node Integer   (MTree a) (MTree a)
             | Leaf Integer a (MTree a) 
             | Empty

data Move a = Next (MTree a) 
            | Level (MTree a)

type Moving a = [Move a]

data Zipper m = Zipper
  { zipper        :: MTree (PVal m)
  , moving        :: Moving (PVal m)
  , oid           :: OID
  , moduleOID     :: OID
  }

type ZipperM m = StateT (Zipper m) m


