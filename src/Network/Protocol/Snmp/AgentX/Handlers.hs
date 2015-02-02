{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.Snmp.AgentX.Handlers where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map
import qualified Data.Label as DL

import Network.Protocol.Snmp.AgentX.MIBTree
import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.Packet
import Network.Protocol.Snmp.AgentX.Types
import Network.Protocol.Snmp (Value(..))

makePdu :: [Either RError VarBind] -> AgentT (Maybe PDU)
makePdu xs = do
  nowref <- sysuptime <$> ask
  now <- liftIO . readMVar $ nowref
  let (good, index, firstBad) = splitByError xs 
  case firstBad of
       Nothing -> return . Just $ Response now NoAgentXError index good
       Just err -> return . Just $ Response now err index good

splitByError :: [Either RError a] -> ([a], Index, Maybe RError)
splitByError xs = 
    case splitByError' xs of
         (xss, Nothing) -> (xss, minBound, Nothing)
         (xss, e) -> (xss, toEnum (1 + length xss), e)
    where
        splitByError' :: [Either RError a] -> ([a], Maybe RError)
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
    route' (TestSet mcontext vbs) = do
        (updatesL, index, firstBad) <- splitByError <$> getUpdateList mcontext vbs
        tr <- transactions <$> ask
        nowref <- sysuptime <$> ask
        now <- liftIO . readMVar $ nowref
        liftIO . modifyMVar_ tr $ return . Map.insert (DL.get tid p) (Transaction mcontext updatesL (map vbl vbs) TestSetT) 
        case firstBad of
             Nothing -> return . Just $ Response now NoAgentXError index vbs
             Just err -> return . Just $ Response now err index (take (fromEnum index) vbs)
        where
        vbl (VarBind _ v) = v
    route' CommitSet = do
        tr <- transactions <$> ask
        st <- Map.lookup (DL.get tid p) <$> (liftIO . readMVar $ tr)
        nowref <- sysuptime <$> ask
        now <- liftIO . readMVar $ nowref
        case st of
             Nothing -> return . Just $ Response now CommitFailed  minBound []
             Just (Transaction mcontext upds vbs TestSetT) -> do
                 liftIO $ mapM_ (\(f, a) -> f mcontext a) $ zip (map saveAIO upds) vbs
                 liftIO $ modifyMVar_ tr $ return . Map.update (const . Just $ Transaction mcontext upds vbs CleanupSetT) (DL.get tid p)
                 return . Just $ Response now NoAgentXError minBound []
             Just _ -> return . Just $ Response now CommitFailed minBound []
    route' CleanupSet = do
        liftIO $ print p
        tr <- transactions <$> ask
        liftIO $ print =<< readMVar tr
        liftIO . modifyMVar_ tr $ return . Map.delete (DL.get tid p) 
        return Nothing
    
    route' _ = do
        liftIO $ print p
        makePdu =<< return [Left RequestDenied]
  

getHandler :: [OID] -> Maybe Context -> AgentT [Either RError VarBind]
getHandler xs mc = fmap Right <$> (unContext mc <$> bridgeToBase (findMany xs))

unContext :: Maybe Context -> [MIB] -> [VarBind]
unContext _ [] = []
unContext Nothing (x:xs) =
    case Map.lookup "" (val x) of
         Just v -> VarBind (oid x) v : unContext Nothing xs
         Nothing -> unContext Nothing xs
unContext (Just c) (x:xs) =
    case (Map.lookup c (val x), Map.lookup "" (val x)) of
         (Just v, _)                 -> VarBind (oid x) v : unContext (Just c) xs
         (_     , Just EndOfMibView) -> VarBind (oid x) EndOfMibView : []
         _ -> unContext (Just c) xs

getNextHandler :: Maybe Context -> [SearchRange] -> AgentT [Either RError VarBind]
getNextHandler mc xs = map Right <$> (unContext mc <$> mapM (bridgeToBase . findNext) xs)

getBulkHandler :: Maybe Context -> NonRepeaters -> MaxRepeaters -> [SearchRange] -> AgentT [Either RError VarBind]
getBulkHandler = undefined

getUpdate :: Maybe Context -> VarBind -> AgentT (Either RError Update)
getUpdate mcontext (VarBind o v) = do
    m <- bridgeToBase (findOne o)  
    liftIO $ print m
    if (isWritable m)
       then do
           r <- liftIO $ checkAIO (upd m) mcontext v
           case r of
                NoAgentXError -> return $ Right (upd m)
                e -> return $ Left e
       else return $ Left NotWritable

getUpdateList :: Maybe Context -> [VarBind] -> AgentT [Either RError Update]
getUpdateList mcontext = mapM (getUpdate mcontext)
         

setHandler :: [VarBind] -> AgentT [Either RError MIB]
setHandler [] = return []
setHandler (x:xs) = (:) <$> setOne x <*> setHandler xs

setOne :: VarBind -> AgentT (Either RError MIB)
setOne (VarBind oid' _value') = do
    m <- bridgeToBase (findOne oid')
    liftIO $ print m
    case (Map.lookup "" (val m), upd m) of
         (Just NoSuchInstance, _) -> return $ Left NotWritable
         (Just NoSuchObject, _) -> return $ Left NotWritable
         (_, Fixed) -> return $ Left NotWritable
         (_, Read _) -> return $ Left NotWritable
         _ -> return $ Right m
