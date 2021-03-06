{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Protocol.Snmp.AgentX.MIBTree.MIBTree 
( initModule
, registerFullTree
, unregisterFullTree
, findOne
, findMany
, findNext
, findClosest
, findManyNext
, regWrapper
, askTree
, regByDiff
)
where

import Data.Maybe 
import Control.Applicative
import Control.Monad.State.Strict (MonadIO, forM_, lift, get, put, liftIO, when)
import Network.Protocol.Snmp.AgentX.MIBTree.Types hiding (context)
import Network.Protocol.Snmp.AgentX.MIBTree.Tree 
import Network.Protocol.Snmp (OID, Value(EndOfMibView, NoSuchInstance, NoSuchObject))
import Network.Protocol.Snmp.AgentX.Packet (Context, SearchRange, startOID, endOID, include)
import Control.Concurrent.MVar
import qualified Data.Label as L
import Data.List (stripPrefix)

import Data.Monoid
import Data.Label.Monadic
import Control.Category ((.))
import Prelude hiding ((.))
-- import Debug.Trace

-- | build tree and init module
initModule :: MIBTree IO ()
initModule = flip forM_ evalTree =<< toUpdateList  <$> gets ou  
    where
    evalTree :: MIB -> MIBTree IO ()
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
                Module (z,_) (o,_) _ _ <- get
                put old
                modify zipper $ top . attach z
                modify ou $ top . attach o

-- | register all MIBs in snmp server
registerFullTree :: (Monad m, MonadIO m, Functor m) => MIBTree m ()
registerFullTree = do
    z <- fst . top <$> gets zipper 
    mv <- gets register
    b <- gets moduleOID
    liftIO $ putMVar mv (addBaseOid b $ regPair z Empty)

unregisterFullTree :: (Monad m, MonadIO m, Functor m) => MIBTree m ()
unregisterFullTree = do
    z <- fst . top <$> gets zipper 
    mv <- gets register
    b <- gets moduleOID
    liftIO $ putMVar mv (addBaseOid b $ regPair Empty z)

askTree :: (Monad m, MonadIO m, Functor m) => MIBTree m (Tree IValue)
askTree = fst . top <$> gets zipper

regByDiff :: (Monad m, MonadIO m, Functor m) => Tree IValue -> Tree IValue -> MIBTree m ()
regByDiff old new = do
    mv <- gets register
    b <- gets moduleOID
    liftIO $ putMVar mv (addBaseOid b $ regPair old new)


addBaseOid :: OID -> ([(OID, Maybe Context)], [(OID, Maybe Context)]) -> ([(OID, Maybe Context)], [(OID, Maybe Context)])
addBaseOid b (reg, unreg) = (map fun reg, map fun unreg)
    where
    fun (o, mc) = (b <> o, mc)

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

inRange :: SearchRange -> MIB -> MIB  
inRange s m =
    if (L.get startOID s) <= oi m && oi m < (L.get endOID s)
        then ObjectType (oi m) (last $ oi m) "" "" Nothing (val m)
        else ObjectType (L.get startOID s) (last $ L.get startOID s) "" "" Nothing (rsValue EndOfMibView)

-- | find one MIB 
findOne :: 
      OID  -- ^ path for find
    -> Maybe Context -- ^ context, you can have many values with one path and different context
    -> MIBTree IO MIB 
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
             findOne' ys' <$> gets zipper
    where
      findOne' xs z = toObject $ setCursor xs mcontext z

      toObject Nothing = ObjectType ys (last ys) "" "" mcontext nsi
      toObject (Just (Node (Contexted (i, _, v)) _ Empty, _)) = ObjectType ys i "" "" mcontext (fromMaybe nso v)
      toObject _ = ObjectType ys (last ys) "" "" mcontext nso

      nso, nsi :: PVal 
      nso = rsValue NoSuchObject
      nsi = rsValue NoSuchInstance

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

-- | wrap MIBTree action, get MIB tree before and after, register added mibs, unregister removed mibs
regWrapper :: (Monad m, MonadIO m, Functor m) => MIBTree m x -> MIBTree m x
regWrapper x = do
    old <- fst . top <$> gets zipper  
    r <- x
    new <- fst . top <$> gets zipper  
    mv <- gets register
    b <- gets moduleOID
    let diff = addBaseOid b $ regPair new old
    when (diff /= ([], [])) $ do
        liftIO $ putMVar mv diff
    return r


-- | like findOne, but for many paths
findMany :: [OID] -> Maybe Context -> MIBTree IO [MIB]
findMany xs mc = mapM (flip findOne mc) xs

-- | find next node in MIBTree 
findNext :: 
      SearchRange  -- ^ SearchRange (getwalk or getnext requests)
    -> Maybe Context -- ^ context
    -> MIBTree IO MIB -- ^ search result
findNext sr mcontext = do
    modify zipper top
    modify ou top
    modOID <- gets moduleOID
    let start = L.get startOID sr
        end   = L.get endOID sr
    case (stripPrefix modOID start, stripPrefix modOID end) of
         (Nothing, _) -> return $ ObjectType start (last start) "" "" mcontext eom
         (Just start', Just end') -> do
             updates <- gets ou
             puts ou (updateSubtree start' updates)
             initModule
             puts ou updates
             let fixSearchRange = L.set startOID start' . L.set endOID end' 
             fixMib modOID . findNext' (fixSearchRange sr) <$> gets zipper
         _ -> error "findNext"

    where
    eom :: PVal
    eom = rsValue EndOfMibView

    fixMib :: OID -> MIB -> MIB
    fixMib m (ObjectType o i _ _ mc v) = ObjectType (m <> o) i "" "" mc v
    fixMib _ _ = error "fixMib"

    findNext' :: SearchRange -> Zipper Tree IValue -> MIB
    findNext' sr' z 
      | L.get include sr' =
          let start = L.get startOID sr'
              nz@(Node v _ _, _) = goClosest start mcontext z 
              o = oid nz
              Contexted (i, mc, Just pv) = v
          in if o == start && withValue v && mc == mcontext
                then ObjectType o i "" "" mc pv
                else findNext' (L.set include False $ sr') z
      | otherwise =
          let start = L.get startOID sr'
              nz = goClosest start mcontext z
              l = hasLevel nz
              n = hasNext nz
          in case (l, n) of
                  (True, _) -> inRange sr' $ findClosest False  start mcontext (fromJust $ goLevel nz)
                  (False, True) -> inRange sr' $ findClosest False start  mcontext (fromJust $ goNext nz)
                  (False, False) -> inRange sr' $ findClosest True start  mcontext (fromJust $ goUp nz)

findClosest :: Bool -> OID -> Maybe Context -> Zipper Tree IValue -> MIB
findClosest back o mcontext z =
    let (canBeObject, checkContextEquality) = isFocusObjectType z
        magic = if back 
                 then not (hasLevel z)
                 else (hasLevel z)
        isNextAvailable = hasNext z
    in case (canBeObject, checkContextEquality mcontext, magic, isNextAvailable) of
            (True,  True, _,     _    ) -> getFocus z 
            (_,     _,    True,  _    ) -> findClosest False o mcontext (fromJust $ goLevel z)
            (_,     _,    False, True ) -> findClosest False o mcontext (fromJust $ goNext z)
            _                           -> case goUp z of
                                           Just nz -> findClosest True o mcontext nz
                                           Nothing -> ObjectType o (last o) "" "" Nothing (rsValue EndOfMibView)

isFocusObjectType :: Contexted a => (Tree a, t) -> (Bool, Maybe Context -> Bool)
isFocusObjectType (Node v _ Empty,_) = (withValue v, (==) (context v))
isFocusObjectType _ = (False, const False)

getFocus :: Zipper Tree IValue -> MIB
getFocus z@(Node (Contexted (i, mc, Just v)) _ _, _) = 
    let o = oid z
    in ObjectType o i "" "" mc v
getFocus _ = error "getFocus"

-- | like findNext
findManyNext :: [SearchRange] -> Maybe Context -> MIBTree IO [MIB]
findManyNext xs mc = mapM (flip findNext mc) xs


