{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Protocol.Snmp.AgentX.Packet.Binary where

import Data.Binary
import Network.Protocol.Snmp.AgentX.Packet.Types ( Packet(..), SessionID(..), TransactionID(..), PacketID(..) )
import Network.Protocol.Snmp.AgentX.Packet.Get
import Control.Applicative ((<$>))

instance Binary Packet where
    put = undefined 
    get = do
        version <- getWord8 
        pduTag <- getWord8
        flags <- decodeFlags <$> getWord8
        _reserved <- getWord8
        sid <- get32 flags 
        tid <- get32 flags 
        pid <- get32 flags 
        bodySize <- get32 flags 
        pdu <- parsePdu pduTag flags bodySize
        return $ Packet version pdu flags (SessionID sid) (TransactionID tid) (PacketID pid)


