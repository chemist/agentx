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

{--
7.2.3.2.  Subagent Processing of the agentx-GetNext-PDU

    Upon the subagent's receipt of an agentx-GetNext-PDU, each
    SearchRange in the request is processed as follows:

    (1)  The subagent searches for a variable within the
         lexicographically ordered list of variable names for all
         variables it instantiates (without regard to registration of
         regions) within the indicated context and session, as follows:

         -  if the "include" field of the starting OID is 0, the
            variable's name is the closest lexicographical successor to
            the starting OID.

         -  if the "include" field of the starting OID is 1, the
            variable's name is either equal to, or the closest
            lexicographical successor to, the starting OID.

         -  If the ending OID is not null, the variable's name
            lexicographically precedes the ending OID.

         If a variable is successfully located, v.name is set to that
         variable's name.  v.type and v.data are encoded to represent the
         variable's syntax and value, as described in section 5.4, "Value
         Representation".

    (2)  If the subagent cannot locate an appropriate variable, v.name is
         set to the starting OID, and the VarBind is set to `
         endOfMibView', in the manner described in section 5.4, "Value
         Representation".
--}

getNextHandler :: [SearchRange] -> AgentT [MIB]
getNextHandler [] = return []
getNextHandler (x:xs) = (:) <$> bridgeToBase (findNext x) <*> getNextHandler xs
