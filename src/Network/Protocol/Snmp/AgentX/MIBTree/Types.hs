{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
module Network.Protocol.Snmp.AgentX.MIBTree.Types where

import Control.Monad.State.Strict 
import Control.Concurrent.MVar
import Data.Monoid ((<>))
import Data.Foldable (foldMap)
import Data.Label

import Network.Protocol.Snmp.AgentX.MIBTree.Tree 
import Network.Protocol.Snmp.AgentX.MIBTree.MIB 
import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Packet (Context, CommitError, TestError, UndoError)


data PVal m = Read 
            { readAIO        :: m Value 
            }
          | ReadWrite 
            { readAIO        :: m Value
            , commitSetAIO   :: Value -> m CommitError
            , testSetAIO     :: Value -> m TestError
            , undoSetAIO     :: Value -> m UndoError
            }

rsValue :: (Monad m, MonadIO m, Functor m) => Value -> PVal m
rsValue v = Read $ return v

rdValue :: (Monad m, MonadIO m, Functor m) => m Value -> PVal m
rdValue = Read

rwValue :: (Monad m, MonadIO m, Functor m) => m Value -> (Value -> m CommitError) -> (Value -> m TestError) -> (Value -> m UndoError) -> PVal m
rwValue = ReadWrite

isWritable :: (Monad m, MonadIO m, Functor m) => PVal m -> Bool
isWritable ReadWrite{} = True
isWritable _ = False

instance (Monad m, MonadIO m, Functor m) => Show (PVal m) where
    show Read{} = "Read Value"
    show ReadWrite{} = "ReadWrite Value"


type IMIB m = MIB m (PVal m)

-- newtype ICV a = ICV (Integer, Maybe Context, Maybe a) 
newtype ContextedValue a = Contexted { unContext :: (Integer, Maybe Context, Maybe a) }

type IValue m = ContextedValue (PVal m) 
type IUpdate m = ContextedValue (Update m (PVal m))

type IVal m = m (PVal m)

instance Contexted (ContextedValue a) where
    index (Contexted (i, _, _)) = i
    context (Contexted (_, c, _)) = c
    withValue (Contexted (_, _, Just _)) = True
    withValue _ = False

toC :: Integer -> Maybe Context -> Maybe a -> ContextedValue a
toC i mc mv = Contexted (i, mc, mv)

zero :: Integer -> ContextedValue a
zero i = Contexted (i, Nothing, Nothing)

instance Show a => Show (ContextedValue a) where
    show (Contexted (_, Nothing, Nothing)) = "- node -"
    show (Contexted (_, Nothing, Just v)) = "- leaf " <> show v
    show (Contexted (_, Just c, Just v)) = "- contexted leaf " <> show c <> show v
    show _ = "bad node"


data Module m = Module
  { _zipper        :: Zipper Tree (IValue m)
  , _ou            :: Zipper Tree (IUpdate m)
  , _moduleOID     :: OID
  , _findOid       :: OID
  , _register      :: MVar [IMIB m]
  , _unregister    :: MVar [IMIB m]
  } 

mkLabel ''Module

instance (Monad m, MonadIO m, Functor m) => Show (Module m) where
    show (Module z ou' _ _ _ _) = show z ++ "\n" ++ show ou'


type MIBTree m = StateT (Module m) m

mkModule :: (Monad m, MonadIO m, Functor m) => OID -> [IMIB m] -> m (Module m)
mkModule moduleOid mibs = do
    r <- liftIO $ newEmptyMVar
    unr <- liftIO $ newEmptyMVar
    return $ Module (toZipper . fst . buildTree $ mibs) (toZipper . snd . buildTree $ mibs) moduleOid [] r unr

buildTree :: (Monad m, MonadIO m, Functor m) => [IMIB m] -> (Tree (IValue m), Tree (IUpdate m))
buildTree ms = foldMap singleton $ fillOid ms
  where
    singleton :: (Monad m, MonadIO m) => IMIB m -> (Tree (IValue m), Tree (IUpdate m))
    singleton m = singleton' (oi m, m)
      where
        singleton' :: (OID, IMIB m) -> (Tree (IValue m), Tree (IUpdate m))
        singleton' ([],  _) = (Empty, Empty)
        singleton' ([_], Object _ i _ _ Nothing) = (Node (zero i) Empty Empty, Empty )
        singleton' ([_], Object _ i _ _ u@_) = (Node (zero i) Empty Empty, Node (toC i Nothing u) Empty Empty )
        singleton' ([_], ObjectType _ i _ _ c v) = (Node (toC i c (Just v)) Empty Empty, Empty)
        singleton' ((i:xs), obj@(Object _ _ _ _ Nothing)) = (Node (zero i) Empty (fst $ singleton' (xs, obj)), Empty)
        singleton' ((i:xs), obj@(Object _ _ _ _ _)) = (Node (zero i) Empty (fst $ singleton' (xs, obj)), Node (zero i) Empty (snd $ singleton' (xs, obj)))
        singleton' ((i:xs), obj@(ObjectType{})) = (Node (zero i) Empty (fst $ singleton' (xs, obj)), Empty)


