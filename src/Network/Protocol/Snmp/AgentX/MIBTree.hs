{-# LANGUAGE RankNTypes #-}
module Network.Protocol.Snmp.AgentX.MIBTree 
( MIBTree
, PVal(..)
, IVal
, Module
, findOne
, findMany
, findNext
, findManyNext
, MIB
, IMIB
, isWritable
, mkModule
, mkObject
, mkObjectType
, toRegistrationList
, isObjectType
, val
, oi
, context
, moduleOID
, register
, unregister
, initModule
, initAndRegister
, zipper
, rsValue
, rdValue
, rwValue
, Update(..)
, IUpdate
, IValue
, UpdateM
, ValueM
, MIBM 
)
where
import Network.Protocol.Snmp.AgentX.MIBTree.MIBTree
import Network.Protocol.Snmp.AgentX.MIBTree.MIB
import Network.Protocol.Snmp.AgentX.MIBTree.Types
import Control.Monad.IO.Class (MonadIO)

type UpdateM = forall m. (Monad m, MonadIO m, Functor m) => Update m (PVal m)
type ValueM = forall m . (Monad m, MonadIO m, Functor m) => PVal m
type MIBM = forall m . (Monad m, MonadIO m, Functor m) => IMIB m

