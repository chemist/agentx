module Network.Protocol.Snmp.AgentX.Handlers where

import Control.Applicative
import Control.Monad.State

import Network.Protocol.Snmp.AgentX.MIBTree
import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.Protocol hiding (getValue)
import Network.Protocol.Snmp.AgentX.Types

makePdu :: [MIB] -> AgentT PDU
makePdu xs = do
  now <- sysuptime <$> get
  return $ Response now NoAgentXError (Index 0) $ map mibToVarBind xs

mibToVarBind :: MIB -> VarBind
mibToVarBind y = VarBind (oid y) (val y)

route :: Packet -> AgentT Packet
route p@(Packet _ pdu _ _ _ _) = do
    liftIO $ print pdu
    pdu' <- makePdu =<< route'    
    let (Packet v _ flags sid tid pid) = p
    return $ Packet v pdu' flags sid tid pid
    where
      route' = case pdu of
                    Get _ oids -> getHandler oids
                    GetNext _ srange -> getNextHandler srange
                    _ -> undefined

getHandler :: [OID] -> AgentT [MIB]
getHandler xs = bridgeToBase (findMany xs) 

getNextHandler :: [SearchRange] -> AgentT [MIB]
getNextHandler [] = return []
getNextHandler (x:xs) = (:) <$> bridgeToBase (findNext x) <*> getNextHandler xs
