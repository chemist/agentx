{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
module Network.Protocol.Snmp.AgentX.MIBTree.Types where

import Control.Monad.State.Strict 
-- import Control.Concurrent.MVar
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

rsValue :: (Monad m, MonadIO m) => Value -> PVal m
rsValue v = Read $ return v

rdValue :: (Monad m, MonadIO m) => m Value -> PVal m
rdValue = Read

rwValue :: (Monad m, MonadIO m) => m Value -> (Value -> m CommitError) -> (Value -> m TestError) -> (Value -> m UndoError) -> PVal m
rwValue = ReadWrite

instance (Monad m, MonadIO m) => Show (PVal m) where
    show Read{} = "Read Value"
    show ReadWrite{} = "ReadWrite Value"


type MIBM m = MIB m (PVal m)

newtype ICV a = ICV (Integer, Maybe Context, Maybe a) 

instance Show a => Show (ICV a) where
    show (ICV (_, Nothing, Nothing)) = "- node -"
    show (ICV (_, Nothing, Just v)) = "- leaf " <> show v
    show (ICV (_, Just c, Just v)) = "- contexted leaf " <> show c <> show v
    show _ = "bad node"


instance HasIndex (ICV a) where
    index (ICV (i, _, _)) = i
    split (ICV (i, x, y)) (ICV (_, x1, y1)) = ICV (i, x `fun` x1, y `fun` y1)
      where
      fun Nothing a = a
      fun a Nothing = a
      fun _ _ = error "HasIndex (ICV)"
    withValue (ICV (_, _, Just _)) = True
    withValue _ = False
    context (ICV (_, c, _)) = c
    

type UpdateM m = Update m (PVal m)

data Module m a = Module
  { _zipper        :: Zipper Tree (ICV a)
  , _ou            :: Zipper Tree (ICV (Update m a))
  , _moduleOID     :: OID
  } 

mkLabel ''Module

instance Show a => Show (Module m a) where
    show (Module z ou' _) = show z ++ "\n" ++ show ou'


type MIBTree m a = StateT (Module m a) m

mkModule :: (Monad m, MonadIO m) => OID -> [MIB m a] -> Module m a
mkModule moduleOid mibs = Module (toZipper . fst . buildTree $ mibs) (toZipper . snd . buildTree $ mibs) moduleOid

buildTree :: (Monad m, MonadIO m) => [MIB m a] -> (Tree (ICV a), Tree (ICV (Update m a)))
buildTree ms = foldMap singleton $ fillOid ms
  where
    singleton :: (Monad m, MonadIO m) => MIB m a -> (Tree (ICV a), Tree (ICV (Update m a)))
    singleton m = singleton' (oi m, m)
      where
        zero :: Integer -> ICV a
        zero i = ICV (i, Nothing, Nothing)
        singleton' :: (OID, MIB m a) -> (Tree (ICV a), Tree (ICV (Update m a)))
        singleton' ([],  _) = (Empty, Empty)
        singleton' ([_], Object _ i _ _ Nothing) = (Node (zero i) Empty Empty, Empty )
        singleton' ([_], Object _ i _ _ (Just u)) = (Node (zero i) Empty Empty, Node (ICV (i, Nothing, Just u)) Empty Empty )
        singleton' ([_], ObjectType _ i _ _ c v) = (Node (ICV (i, c, Just v)) Empty Empty, Empty)
        singleton' ((i:xs), obj@(Object _ _ _ _ Nothing)) = (Node (zero i) Empty (fst $ singleton' (xs, obj)), Empty)
        singleton' ((i:xs), obj@(Object _ _ _ _ (Just _))) = (Node (zero i) Empty (fst $ singleton' (xs, obj)), Node (zero i) Empty (snd $ singleton' (xs, obj)))
        singleton' ((i:xs), obj@(ObjectType{})) = (Node (zero i) Empty (fst $ singleton' (xs, obj)), Empty)


