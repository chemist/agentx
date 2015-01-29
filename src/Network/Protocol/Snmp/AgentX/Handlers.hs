{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.Snmp.AgentX.Handlers where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map
import Data.Maybe

import Network.Protocol.Snmp.AgentX.MIBTree
import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.Protocol hiding (getValue)
import Network.Protocol.Snmp.AgentX.Types
import Network.Protocol.Snmp (Value(..))

makePdu :: [Either RError MIB] -> AgentT (Maybe PDU)
makePdu xs = do
  nowref <- sysuptime <$> ask
  now <- liftIO . readMVar $ nowref
  let (good, index, firstBad) = splitByError xs 
  case firstBad of
       Nothing -> return . Just $ Response now NoAgentXError index $ map mibToVarBind good
       Just err -> return . Just $ Response now err index $ map mibToVarBind good


splitByError :: [Either RError a] -> ([a], Index, Maybe RError)
splitByError xs = 
    case splitByError' xs of
         (xss, Nothing) -> (xss, Index 0, Nothing)
         (xss, e) -> (xss, toEnum (1 + length xss), e)
    where
        splitByError' :: [Either RError a] -> ([a], Maybe RError)
        splitByError' [] = ([], Nothing)
        splitByError' (Left err : _) = ([], Just err)
        splitByError' (Right x : xs') = 
            let splitted = splitByError' xs'
            in (x : fst splitted, snd splitted)

mibToVarBind :: MIB -> VarBind
mibToVarBind y = VarBind (oid y) (unDefault $ val y)

unDefault :: ContextedValue -> Value
unDefault x = case Map.lookup "" x of
                   Just v -> v
                   Nothing -> error "unDefault"

route :: Packet -> AgentT (Maybe Packet)
route p = route' p (getPdu p) >>= return . fmap (setPdu p)

getPdu :: Packet -> PDU
getPdu (Packet _ pdu _ _ _ _) = pdu


setPdu :: Packet -> PDU -> Packet
setPdu (Packet v _ flags sid tid pid) pdu = Packet v pdu flags sid tid pid

route' :: Packet -> PDU -> AgentT (Maybe PDU)
route' _ (Get mcontext oids) = makePdu =<< getHandler oids mcontext
route' _ (GetNext mcontext srange) = makePdu =<< getNextHandler mcontext srange 
route' _ (GetBulk mcontext nonRepeaters maxRepeaters srange) = makePdu =<< getBulkHandler mcontext nonRepeaters maxRepeaters srange 
route' p (TestSet mcontext vbs) = do
    (updatesL, index, firstBad) <- splitByError <$> getUpdateList mcontext vbs
    tr <- transactions <$> ask
    nowref <- sysuptime <$> ask
    now <- liftIO . readMVar $ nowref
    liftIO . modifyMVar_ tr $ return . Map.insert (transaction p) (Transaction mcontext updatesL (map vbl vbs) TestSetT) 
    case firstBad of
         Nothing -> return . Just $ Response now NoAgentXError index vbs
         Just err -> return . Just $ Response now err index (take (fromEnum index) vbs)
    where
    vbl (VarBind _ v) = v
route' p CommitSet = do
    tr <- transactions <$> ask
    st <- Map.lookup (transaction p) <$> (liftIO . readMVar $ tr)
    nowref <- sysuptime <$> ask
    now <- liftIO . readMVar $ nowref
    case st of
         Nothing -> return . Just $ Response now CommitFailed  (Index 0) []
         Just (Transaction mcontext upds vbs TestSetT) -> do
             liftIO $ mapM_ (\(f, a) -> f mcontext a) $ zip (map saveAIO upds) vbs
             liftIO $ modifyMVar_ tr $ return . Map.update (const . Just $ Transaction mcontext upds vbs CleanupSetT) (transaction p)
             return . Just $ Response now NoAgentXError (Index 0) []
         Just _ -> return . Just $ Response now CommitFailed (Index 0) []
route' p CleanupSet = do
    liftIO $ print p
    tr <- transactions <$> ask
    liftIO $ print =<< readMVar tr
    liftIO . modifyMVar_ tr $ return . Map.delete (transaction p) 
    return Nothing

route' p _ = do
    liftIO $ print p
    makePdu =<< return [Left RequestDenied]

transaction :: Packet -> TransactionID
transaction (Packet _ _ _ _ t _) = t

getHandler :: [OID] -> Maybe Context -> AgentT [Either RError MIB]
getHandler xs mcontext = fmap Right <$> (filterByContext mcontext <$> bridgeToBase (findMany xs))

filterByContext :: Maybe Context -> [MIB] -> [MIB]
filterByContext c xs = filter fun xs
    where
    fun x = case Map.lookup (fromMaybe "" c)  (val x) of
                    Nothing -> False
                    _ -> True
            


getNextHandler :: Maybe Context -> [SearchRange] -> AgentT [Either RError MIB]
getNextHandler mcontext xs = 
    let filtered = filterByContext mcontext <$> mapM (bridgeToBase . findNext) xs
    in fmap Right <$> filtered

getBulkHandler :: Maybe Context -> NonRepeaters -> MaxRepeaters -> [SearchRange] -> AgentT [Either RError MIB]
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
