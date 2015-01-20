module Network.Protocol.Snmp.AgentX.Handlers where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map

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
mibToVarBind y = VarBind (oid y) (val y)

route :: Packet -> AgentT (Maybe Packet)
route p = route' p (getPdu p) >>= return . fmap (setPdu p)

getPdu :: Packet -> PDU
getPdu (Packet _ pdu _ _ _ _) = pdu

setPdu :: Packet -> PDU -> Packet
setPdu (Packet v _ flags sid tid pid) pdu = Packet v pdu flags sid tid pid

route' :: Packet -> PDU -> AgentT (Maybe PDU)
route' _ (Get _ oids) = makePdu =<< getHandler oids
route' _ (GetNext _ srange) = makePdu =<< getNextHandler srange
route' _ (GetBulk _ nonRepeaters maxRepeaters srange) = makePdu =<< getBulkHandler nonRepeaters maxRepeaters srange
route' p (TestSet _ vbs) = do
    (updatesL, index, firstBad) <- splitByError <$> getUpdateList vbs
    tr <- transactions <$> ask
    nowref <- sysuptime <$> ask
    now <- liftIO . readMVar $ nowref
    liftIO . modifyMVar_ tr $ return . Map.insert (transaction p) (Transaction updatesL (map vbl vbs) TestSetT) 
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
         Just (Transaction upds vbs TestSetT) -> do
             liftIO $ mapM_ (\(f, a) -> f a) $ zip (map saveAIO upds) vbs
             liftIO $ modifyMVar_ tr $ return . Map.update (const . Just $ Transaction upds vbs CleanupSetT) (transaction p)
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

getHandler :: [OID] -> AgentT [Either RError MIB]
getHandler xs = fmap Right <$> bridgeToBase (findMany xs) 

getNextHandler :: [SearchRange] -> AgentT [Either RError MIB]
getNextHandler = mapM $ fmap Right . bridgeToBase . findNext 

getBulkHandler :: NonRepeaters -> MaxRepeaters -> [SearchRange] -> AgentT [Either RError MIB]
getBulkHandler = undefined

getUpdate :: VarBind -> AgentT (Either RError Update)
getUpdate (VarBind o v) = do
    m <- bridgeToBase (findOne o)  
    liftIO $ print m
    if (isWritable m)
       then do
           r <- liftIO $ checkAIO (upd m) v
           case r of
                NoAgentXError -> return $ Right (upd m)
                e -> return $ Left e
       else return $ Left NotWritable

getUpdateList :: [VarBind] -> AgentT [Either RError Update]
getUpdateList = mapM getUpdate 
         

setHandler :: [VarBind] -> AgentT [Either RError MIB]
setHandler [] = return []
setHandler (x:xs) = (:) <$> setOne x <*> setHandler xs

setOne :: VarBind -> AgentT (Either RError MIB)
setOne (VarBind oid' _value') = do
    m <- bridgeToBase (findOne oid')
    liftIO $ print m
    case (val m, upd m) of
         (NoSuchInstance, _) -> return $ Left NotWritable
         (NoSuchObject, _) -> return $ Left NotWritable
         (_, Fixed) -> return $ Left NotWritable
         (_, Read _) -> return $ Left NotWritable
         _ -> return $ Right m
