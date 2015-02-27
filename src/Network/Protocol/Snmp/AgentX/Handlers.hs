{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.Snmp.AgentX.Handlers where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.MVar
import qualified Data.Label as DL

import Network.Protocol.Snmp.AgentX.MIBTree
import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.Packet
import Network.Protocol.Snmp.AgentX.Types

makePdu :: [Either TaggedError VarBind] -> AgentT (Maybe PDU)
makePdu xs = do
  nowref <- sysuptime <$> ask
  now <- liftIO . readMVar $ nowref
  let (good, index, firstBad) = splitByError xs 
  case firstBad of
       Nothing -> return . Just $ Response now (Tagged NoAgentXError) index good
       Just err -> return . Just $ Response now err index good

splitByError :: [Either TaggedError a] -> ([a], Index, Maybe TaggedError)
splitByError xs = 
    case splitByError' xs of
         (xss, Nothing) -> (xss, minBound, Nothing)
         (xss, e) -> (xss, toEnum (1 + length xss), e)
    where
        splitByError' :: [Either TaggedError a] -> ([a], Maybe TaggedError)
        splitByError' [] = ([], Nothing)
        splitByError' (Left err : _) = ([], Just err)
        splitByError' (Right x : xs') = 
            let splitted = splitByError' xs'
            in (x : fst splitted, snd splitted)

route :: Packet -> AgentT (Maybe Packet)
route p = route' (DL.get pdu p) >>= return . fmap (flip (DL.set pdu) p)
  where
    route' :: PDU -> AgentT (Maybe PDU)
    route' (Get mcontext oids) = makePdu =<< getHandler oids mcontext
    route' (GetNext mcontext srange) = makePdu =<< getNextHandler mcontext srange 
    route' (GetBulk mcontext nonRepeaters maxRepeaters srange) = makePdu =<< getBulkHandler mcontext nonRepeaters maxRepeaters srange 
    {--
    route' (TestSet mcontext vbs) = do
        (updatesL, index, firstBad) <- splitByError <$> getUpdateList mcontext vbs
        tr <- transactions <$> ask
        nowref <- sysuptime <$> ask
        now <- liftIO . readMVar $ nowref
        liftIO . modifyMVar_ tr $ return . Map.insert (DL.get tid p) (Transaction mcontext updatesL (map vbl vbs) TestSetT) 
        case firstBad of
             Nothing -> return . Just $ Response now (Tagged NoAgentXError) index vbs
             Just err -> return . Just $ Response now err index (take (fromEnum index) vbs)
        where
        vbl (VarBind _ v) = v
    route' CommitSet = do
        tr <- transactions <$> ask
        st <- Map.lookup (DL.get tid p) <$> (liftIO . readMVar $ tr)
        nowref <- sysuptime <$> ask
        now <- liftIO . readMVar $ nowref
        case st of
             Nothing -> return . Just $ Response now (Tagged CommitFailed)  minBound []
             Just (Transaction mcontext upds vbs TestSetT) -> do
                 liftIO $ mapM_ (\(f, a) -> f mcontext a) $ zip (map commitSetAIO upds) vbs
                 liftIO $ modifyMVar_ tr $ return . Map.update (const . Just $ Transaction mcontext upds vbs CleanupSetT) (DL.get tid p)
                 return . Just $ Response now (Tagged NoCommitError) minBound []
             Just _ -> return . Just $ Response now (Tagged CommitFailed) minBound []
    route' CleanupSet = do
        liftIO $ print p
        tr <- transactions <$> ask
        liftIO $ print =<< readMVar tr
        liftIO . modifyMVar_ tr $ return . Map.delete (DL.get tid p) 
        return Nothing
        --}
    
    route' _ = do
        liftIO $ print p
        makePdu =<< return [Left (Tagged RequestDenied)]
  

getHandler :: [OID] -> Maybe Context -> AgentT [Either TaggedError VarBind]
getHandler xs mc = map Right <$> (liftIO . mapM mibToVarBind =<< bridgeToBase (findMany xs mc))


getNextHandler :: Maybe Context -> [SearchRange] -> AgentT [Either TaggedError VarBind]
getNextHandler mc xs = map Right <$> (liftIO . mapM mibToVarBind =<< bridgeToBase (findManyNext xs mc))

getBulkHandler :: Maybe Context -> NonRepeaters -> MaxRepeaters -> [SearchRange] -> AgentT [Either TaggedError VarBind]
getBulkHandler = undefined

mibToVarBind :: (Monad m, MonadIO m, Functor m) => IMIB m -> m VarBind
mibToVarBind m = do
    v <- readAIO (val m) 
    return $ VarBind (oi m) v

{--
getUpdate :: Maybe Context -> VarBind -> AgentT (Either TaggedError Update)
getUpdate mc (VarBind o v) = do
    m <- bridgeToBase (findOne mc o)  
    liftIO $ print m
    if (isWritable m)
       then do
           r <- liftIO $ testSetAIO (upd m) mcontext v
           case r of
                NoTestError -> return $ Right (upd m)
                e -> return $ Left (Tagged e)
       else return $ Left (Tagged NotWritable)

getUpdateList :: Maybe Context -> [VarBind] -> AgentT [Either TaggedError Update]
getUpdateList mcontext = mapM (getUpdate mcontext)
         

setHandler :: [VarBind] -> AgentT [Either TaggedError MIB]
setHandler [] = return []
setHandler (x:xs) = (:) <$> setOne x <*> setHandler xs

setOne :: VarBind -> AgentT (Either TaggedError MIB)
setOne (VarBind oid' _value') = do
    m <- bridgeToBase (findOne oid')
    liftIO $ print m
    case (Map.lookup "" (val m), upd m) of
         (Just NoSuchInstance, _) -> return $ Left (Tagged NotWritable)
         (Just NoSuchObject, _) -> return $ Left (Tagged NotWritable)
         (_, Fixed) -> return $ Left (Tagged NotWritable)
         (_, Read _) -> return $ Left (Tagged NotWritable)
         _ -> return $ Right m

--}
