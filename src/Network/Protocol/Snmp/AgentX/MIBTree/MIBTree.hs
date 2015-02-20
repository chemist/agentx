{-# LANGUAGE FlexibleContexts #-}
module Network.Protocol.Snmp.AgentX.MIBTree.MIBTree where

import Data.Maybe 
import Control.Applicative
import Control.Monad.State.Strict (MonadIO, forM_, lift, get, put)
import Network.Protocol.Snmp.AgentX.MIBTree.Types 
import Network.Protocol.Snmp.AgentX.MIBTree.Tree
import Network.Protocol.Snmp.AgentX.MIBTree.MIB  hiding (context)
import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.Packet (Context)

import Data.Monoid
import Data.Label.Monadic
import Control.Category ((.))
import Prelude hiding ((.))

initModule :: (Show a, Monad m, MonadIO m, Functor m) =>  MIBTree m a ()
initModule = flip forM_ evalTree =<< toUpdateList  <$> gets ou  
    where
    evalTree :: (Show a, Monad m, MonadIO m, Functor m) => MIB m a -> MIBTree m a ()
    evalTree obj = do
        (mibs, updates) <- buildTree <$> (lift $ unUpdate . fromJust . update $ obj)
        case updates of
             Empty -> do -- if without updates just attach subtree
                modify zipper $ top . attach mibs . (fromJust . setCursor (oi obj) Nothing) . top
                modify ou $ top . attach updates . (fromJust . setCursor (oi obj) Nothing) . top
             _     -> do -- if with updates, save state, build new subtree, return state, and attach builded subtree
                modify zipper $  fromJust . setCursor (oi obj) Nothing . top
                modify ou $ fromJust . setCursor (oi obj) Nothing . top
                old <- get
                modify zipper $  const (mibs, [])
                modify ou $ const (updates, [])
                initModule
                Module (z,_) (o,_) _ <- get
                put old
                modify zipper $ top . attach z
                modify ou $ top . attach o

toUpdateList :: Zipper Tree (ICV (Update m a)) -> [MIB m a]
toUpdateList (Empty, _) = []
toUpdateList (t, _) = toUpdateList' ([], t)
  where
  toUpdateList' :: (OID, Tree (ICV (Update m a))) -> [MIB m a]
  toUpdateList' (o, Node x next level) = 
      if withValue x
         then Object (reverse $ index x : o) (index x) "" "" (valueFromICV x) 
              : toUpdateList' (o, next) 
              <> toUpdateList' (index x : o, level)
         else toUpdateList' (o, next)
              <> toUpdateList' (index x : o, level)
  toUpdateList' _ = []
  valueFromICV (ICV (_, _, x)) = x

toRegistrationList :: Zipper Tree (ICV a) -> [MIB m a]
toRegistrationList (Empty, _) = []
toRegistrationList (t, _)  = toRegistrationList' ([], t)
  where
  toRegistrationList' :: (OID, Tree (ICV a)) -> [MIB m a]
  toRegistrationList' (o, Node x next level) = 
      if withValue x
         then ObjectType (reverse $ index x : o) (index x) "" "" (context x) (valueFromICV x)
              : toRegistrationList' (o, next)
              <> toRegistrationList' (index x : o, level)
         else toRegistrationList' (o, next)
              <> toRegistrationList' (index x : o, level)
  toRegistrationList' _ = []
  valueFromICV (ICV (_, _, Just x)) = x
  valueFromICV _ = error "toRegistrationList: Opps, you found bug!!!"
                                           
findMany :: [OID] -> Maybe Context -> [MIB m a]
findMany = undefined


