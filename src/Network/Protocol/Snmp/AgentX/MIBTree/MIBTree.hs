{-# LANGUAGE FlexibleContexts #-}
module Network.Protocol.Snmp.AgentX.MIBTree.MIBTree where

import Data.Maybe 
import Control.Applicative
import Control.Monad.State.Strict (MonadIO, forM_, lift, get, put, liftIO)
import Network.Protocol.Snmp.AgentX.MIBTree.Types 
import Network.Protocol.Snmp.AgentX.MIBTree.Tree
import Network.Protocol.Snmp.AgentX.MIBTree.MIB  hiding (context)
import Network.Protocol.Snmp (OID, Value(EndOfMibView, NoSuchInstance, NoSuchObject))
import Network.Protocol.Snmp.AgentX.Packet (Context, SearchRange, startOID, endOID)
import Control.Concurrent.MVar
import qualified Data.Label as L

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
                Module (z,_) (o,_) _ _ _ _ <- get
                put old
                modify zipper $ top . attach z
                modify ou $ top . attach o

initAndRegister :: (Monad m, MonadIO m, Functor m) => MIBTree m (PVal m) ()
initAndRegister = do
    initModule
    Module z _ _ _ mv _<- get 
    liftIO $ putMVar mv (toRegistrationList z)

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
                                           

inRange :: (Monad m, MonadIO m, Functor m) => SearchRange -> MIBM m -> MIBM m 
inRange s m =
    if (L.get startOID s) <= oi m && oi m < (L.get endOID s)
        then ObjectType (oi m) 0 "" "" Nothing (val m)
        else ObjectType (L.get startOID s) 0 "" "" Nothing (rsValue EndOfMibView)


findOne :: (Monad m, MonadIO m, Functor m) => OID -> Maybe Context -> MIBTree m (PVal m) (MIBM m)
findOne ys c = do
    modify findOid (const ys)
    updates <- gets ou
    puts ou (updateSubtree ys updates)
    initModule
    puts ou updates
    findOne' ys c
    where
      findOne' :: (Monad m, MonadIO m, Functor m) => OID -> Maybe Context -> MIBTree m (PVal m) (MIBM m)
      findOne' [] _ = return $ ObjectType [] 0 "" "" Nothing (rsValue NoSuchInstance)
      findOne' (x : []) mc = do
          Just ic <- cursor <$> gets zipper 
          isValue <- hasValue <$> gets zipper
          v <- getValueFromTree <$> gets zipper 
          isNext <- hasNext <$> gets zipper
          o <- gets findOid
          case (ic == (x, mc), isValue, isNext) of
               (True, True, _) -> return $ ObjectType o x "" "" mc v
               (True, False, _) -> return $ ObjectType o x "" "" mc (rsValue NoSuchObject)
               (False, _, True) -> do
                   modify zipper (fromJust . goNext) 
                   findOne' (x : []) mc
               _ -> return $ ObjectType o x "" "" mc (rsValue NoSuchInstance)
      findOne' (x : xs) mc = do
          isNextZipper <- hasNext <$> gets zipper
          isLevelZipper <- hasLevel <$> gets zipper
          Just (i, _) <- cursor <$> gets zipper 
          o <- gets findOid
          case (i == x, isNextZipper, isLevelZipper) of
               (True, _, True) -> do
                   modify zipper (fromJust . goLevel) 
                   findOne' xs mc
               (False, True, _) -> do
                   modify zipper (fromJust . goNext) 
                   findOne' (x : xs) mc
               _ -> return $ ObjectType o x "" "" mc (rsValue NoSuchObject)

      hasValue :: Zipper Tree (ICV a) -> Bool
      hasValue (Node (ICV (_, _, Just _)) _ Empty, _) = True
      hasValue _ = False

      getValueFromTree :: (Monad m, MonadIO m, Functor m) => Zipper Tree (ICV (PVal m)) -> PVal m
      getValueFromTree (Node (ICV (_, _, Just v)) _ _, _) = v
      getValueFromTree _ = error "getValueFromTree"

      updateSubtree :: HasIndex a => OID -> Zipper Tree a -> Zipper Tree a
      updateSubtree xs z =
          let (x, u) = goClosest xs Nothing z
              isLevel (Level _) = True
              isLevel _ = False
              cleanUnused (Level (Node v _ l)) = Level (Node v Empty l)
              cleanUnused _ = error "cleanUnused"
              cleanHead Empty = Empty
              cleanHead (Node v _ l) = Node v Empty l
          in top (cleanHead x, map cleanUnused $ filter isLevel u)

findMany :: [OID] -> Maybe Context -> MIBTree m a [MIB m a]
findMany = undefined

findNext :: SearchRange -> Maybe Context -> MIBTree m a (MIB m a)
findNext = undefined

