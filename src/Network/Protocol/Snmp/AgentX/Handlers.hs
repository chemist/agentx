module Network.Protocol.Snmp.AgentX.Handlers where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.MVar

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

splitByError :: [Either RError MIB] -> ([MIB], Index, Maybe RError)
splitByError xs = 
    case splitByError' xs of
         (xss, Nothing) -> (xss, Index 0, Nothing)
         (xss, e) -> (xss, toEnum (1 + length xss), e)
    where
        splitByError' :: [Either RError MIB] -> ([MIB], Maybe RError)
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
route' p (TestSet _ vbs) = makePdu =<< setHandler vbs
route' p CleanupSet = return Nothing
route' p CommitSet  = return Nothing
route' p _ = do
    liftIO $ print p
    makePdu =<< return [Left RequestDenied]

getHandler :: [OID] -> AgentT [Either RError MIB]
getHandler xs = fmap Right <$> bridgeToBase (findMany xs) 

getNextHandler :: [SearchRange] -> AgentT [Either RError MIB]
getNextHandler [] = return []
getNextHandler (x:xs) = (:) <$> (Right <$> bridgeToBase (findNext x)) <*> getNextHandler xs

getBulkHandler :: NonRepeaters -> MaxRepeaters -> [SearchRange] -> AgentT [Either RError MIB]
getBulkHandler = undefined

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
