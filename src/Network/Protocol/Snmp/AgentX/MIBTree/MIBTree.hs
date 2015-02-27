{-# LANGUAGE FlexibleContexts #-}
module Network.Protocol.Snmp.AgentX.MIBTree.MIBTree where

import Data.Maybe 
import Control.Applicative
import Control.Monad.State.Strict (MonadIO, forM_, lift, get, put, liftIO)
import Network.Protocol.Snmp.AgentX.MIBTree.Types hiding (context)
import Network.Protocol.Snmp.AgentX.MIBTree.Tree 
import Network.Protocol.Snmp (OID, Value(EndOfMibView, NoSuchInstance, NoSuchObject))
import Network.Protocol.Snmp.AgentX.Packet (Context, SearchRange, startOID, endOID)
import Control.Concurrent.MVar
import qualified Data.Label as L
import Data.List (stripPrefix)

import Data.Monoid
import Data.Label.Monadic
import Control.Category ((.))
import Prelude hiding ((.))

initModule :: (Monad m, MonadIO m, Functor m) =>  MIBTree m ()
initModule = flip forM_ evalTree =<< toUpdateList  <$> gets ou  
    where
    evalTree :: (Monad m, MonadIO m, Functor m) => MIB -> MIBTree m ()
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

initAndRegister :: (Monad m, MonadIO m, Functor m) => MIBTree m ()
initAndRegister = do
    initModule
    Module z _ b _ mv _<- get 
    liftIO $ putMVar mv (addBaseOid b $ toRegistrationList z)

addBaseOid :: OID -> [MIB] -> [MIB]
addBaseOid b = map fun
    where
    fun (ObjectType o i _ _ c v) = ObjectType (b <> o) i "" "" c v
    fun _ = error "only objectType can be registered"

toUpdateList :: Zipper Tree IUpdate  -> [MIB]
toUpdateList (Empty, _) = []
toUpdateList (t, _) = toUpdateList' ([], t)
  where
  toUpdateList' :: (OID, Tree IUpdate) -> [MIB]
  toUpdateList' (o, Node x next level) = 
      if withValue x
         then Object (reverse $ index x : o) (index x) "" "" (valueFromContexted x) 
              : toUpdateList' (o, next) 
              <> toUpdateList' (index x : o, level)
         else toUpdateList' (o, next)
              <> toUpdateList' (index x : o, level)
  toUpdateList' _ = []
  valueFromContexted (Contexted (_, _, x)) = x

toRegistrationList :: Zipper Tree IValue  -> [MIB]
toRegistrationList (Empty, _) = []
toRegistrationList (t, _)  = toRegistrationList' ([], t)
  where
  toRegistrationList' :: (OID, Tree IValue) -> [MIB]
  toRegistrationList' (o, Node x next level) = 
      if withValue x
         then ObjectType (reverse $ index x : o) (index x) "" "" (context x) (valueFromContexted x)
              : toRegistrationList' (o, next)
              <> toRegistrationList' (index x : o, level)
         else toRegistrationList' (o, next)
              <> toRegistrationList' (index x : o, level)
  toRegistrationList' _ = []
  valueFromContexted (Contexted (_, _, Just x)) = x
  valueFromContexted _ = error "toRegistrationList: Opps, you found bug!!!"
                                           

inRange :: SearchRange -> MIB -> MIB  
inRange s m =
    if (L.get startOID s) <= oi m && oi m < (L.get endOID s)
        then ObjectType (oi m) 0 "" "" Nothing (val m)
        else ObjectType (L.get startOID s) 0 "" "" Nothing (rsValue EndOfMibView)


findOne :: (Monad m, MonadIO m, Functor m) => OID -> Maybe Context -> MIBTree m MIB 
findOne ys mcontext = do
    -- init zippers
    modify zipper top
    modify ou top
    modOID <- gets moduleOID
    -- strip module prefix
    case stripPrefix modOID ys of
         Nothing -> return $ ObjectType ys (last ys) "" "" mcontext nso
         Just ys' -> do
             updates <- gets ou
             -- put update subtree to state
             puts ou (updateSubtree ys' updates)
             -- update dynamic branches
             initModule
             -- get back full update tree
             puts ou updates
             -- find
             findOne' ys' 
    where
      findOne' :: (Monad m, MonadIO m, Functor m) => OID -> MIBTree m MIB 
      findOne' [] = return $ ObjectType ys 0 "" "" Nothing nsi
      findOne' (x : []) = do
          Just ic <- cursor <$> gets zipper 
          maybeValue <- getValueFromHead <$> gets zipper 
          isNext <- hasNext <$> gets zipper
          case (ic == (x, mcontext), isNext) of
               (True, _) -> return $ ObjectType ys x "" "" mcontext (fromMaybe nso maybeValue)
               (False, True) -> do
                   modify zipper (fromJust . goNext) 
                   findOne' (x : []) 
               _ -> return $ ObjectType ys x "" "" mcontext nsi
      findOne' (x : xs) = do
          isNextZipper <- hasNext <$> gets zipper
          isLevelZipper <- hasLevel <$> gets zipper
          Just (i, _) <- cursor <$> gets zipper 
          case (i == x, isNextZipper, isLevelZipper) of
               (True, _, True) -> do
                   modify zipper (fromJust . goLevel) 
                   findOne' xs 
               (False, True, _) -> do
                   modify zipper (fromJust . goNext) 
                   findOne' (x : xs) 
               _ -> return $ ObjectType ys x "" "" mcontext nso

      nso, nsi :: PVal 
      nso = rsValue NoSuchObject
      nsi = rsValue NoSuchInstance

      getValueFromHead :: Zipper Tree (ContextedValue a) -> Maybe a
      getValueFromHead (Node (Contexted (_, _, v)) _ Empty, _) = v
      getValueFromHead _ = Nothing

      updateSubtree :: Contexted a => OID -> Zipper Tree a -> Zipper Tree a
      updateSubtree xs z =
          let (x, u) = goClosest xs Nothing z
              isLevel (Level _) = True
              isLevel _ = False
              cleanUnused (Level (Node v _ l)) = Level (Node v Empty l)
              cleanUnused _ = error "cleanUnused"
              cleanHead Empty = Empty
              cleanHead (Node v _ l) = Node v Empty l
          in top (cleanHead x, map cleanUnused $ filter isLevel u)

findMany :: (Monad m, MonadIO m, Functor m) => [OID] -> Maybe Context -> MIBTree m [MIB]
findMany xs mc = mapM (flip findOne mc) xs

findNext :: (Monad m, MonadIO m, Functor m) => SearchRange -> Maybe Context -> MIBTree m MIB
findNext = undefined

findManyNext :: (Monad m, MonadIO m, Functor m) => [SearchRange] -> Maybe Context -> MIBTree m [MIB]
findManyNext xs mc = mapM (flip findNext mc) xs

