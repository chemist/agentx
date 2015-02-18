{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
module Network.Protocol.Snmp.AgentX.MIBTree.Types where

import Control.Monad.State.Strict hiding (gets, modify)
-- import Control.Concurrent.MVar
-- import Data.Monoid ((<>))
import Data.Label

import Network.Protocol.Snmp.AgentX.MIBTree.MTree 
import Network.Protocol.Snmp.AgentX.MIBTree.UTree (UTree)
import qualified Network.Protocol.Snmp.AgentX.MIBTree.UTree as U 
import Network.Protocol.Snmp.AgentX.MIBTree.Zipper
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

singleton :: (Monad m, MonadIO m) => MIB m a -> (MTree a, UTree (Update m a))
singleton m = singleton' (oi m, oi m,  m)
  where
    singleton' :: (OID, OID, MIB m a) -> (MTree a, UTree (Update m a))
    singleton' ([], _, _) = (Empty, U.Empty)
    singleton' ([_], _, Object _ i _ _ Nothing) = (Node i Empty Empty, U.Empty )
    singleton' ([_], _, Object _ i _ _ (Just u)) = (Node i Empty Empty, U.Node i (Just u) U.Empty U.Empty )
    singleton' ([_], _, ObjectType _ i _ _ c v) = (Leaf i c v Empty, U.Empty)
    singleton' ((i:xs), o, obj@(Object _ _ _ _ Nothing)) = (Node i Empty (fst $ singleton' (xs, o, obj)), U.Empty)
    singleton' ((i:xs), o, obj@(Object _ _ _ _ (Just _))) = (Node i Empty (fst $ singleton' (xs, o, obj)), U.Node i Nothing U.Empty (snd $ singleton' (xs, o, obj)))
    singleton' ((i:xs), o, obj@(ObjectType{})) = (Node i Empty (fst $ singleton' (xs, o, obj)), U.Empty)

buildTree :: (Monad m, MonadIO m) => [MIB m a] -> (MTree a, UTree (Update m a))
buildTree xs = 
  let mib = map singleton $ fillOid xs
      updates = map singleton $ fillOid xs
  in (foldl1 insert . map fst $ mib, foldl1 insert . map snd $ updates)


type UpdateM m = Update m (PVal m)

data Storage m a = Storage
  { _zipper        :: Zipper MTree a
  , _ou            :: Zipper UTree (Update m a)
  , _moduleOID     :: OID
  } 

instance Show a => Show (Storage m a) where
    show (Storage z ou _) = show z ++ "\n" ++ show ou


mkLabel ''Storage

type ZipperM m a = StateT (Storage m a) m

mkModule :: (Monad m, MonadIO m) => OID -> [MIB m a] -> Storage m a
mkModule o ms = 
  let (tr, u) = buildTree ms
  in Storage (toZipper tr) (toZipper u) o



c1 :: Maybe Context 
c1 = Just "context1"


findMany :: [OID] -> Maybe Context -> [MIB m a]
findMany = undefined


