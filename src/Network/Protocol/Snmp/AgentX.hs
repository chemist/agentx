{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Network.Protocol.Snmp.AgentX where

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Types
import Network.Protocol.Snmp.AgentX.Monads
import Network.Protocol.Snmp.AgentX.Templates
import Debug.Trace


base = [oidGen| base 1.3.6.1.4.1.44729 Fixmon |]

