module Network.Protocol.Snmp.AgentX where

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Types
import Network.Protocol.Snmp.AgentX.Monads
-- import Network.Protocol.Snmp.AgentX.Templates
import Network.Info
import qualified Network.Info as NI
import Data.ByteString.Char8 (pack)
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Applicative
import qualified Data.Tree.Zipper as Zip
import Debug.Trace


fixmon :: IO ATree
fixmon = do
    let b = (base [1,3,6,1,4,1,44729] "Fixmon")
    t <- generateTime
    n <- generateInterfaces
    return $ root b [t, n]

generateTime :: IO ATree
generateTime = do
    t <- flip div' 1 <$> getPOSIXTime
    let time = leaf 0 "time" (TimeTicks (fromIntegral t)) (Update (generateTime, Nothing)) 
    return time 

generateInterfaces :: IO ATree
generateInterfaces = do
    nx <- getNetworkInterfaces
    let baseLeaf = (leaf 1 "interfaces" Zero (Update (generateInterfaces, Nothing)))
        xs = zip [0 .. fromIntegral $ length nx - 1] nx
        indexes = flip map xs $ \(i,_) -> leaf i "" (Integer . fromIntegral $ i) Up
        names = flip map xs $ \(i, o) -> leaf i "name" (String . pack . NI.name $ o) Up
        ipv4s = flip map xs $ \(i, o) -> leaf i "ipv4" (String . pack . show . NI.ipv4 $ o) Up
        ipv6s = flip map xs $ \(i, o) -> leaf i "ipv6" (String . pack . show . NI.ipv6 $ o) Up
        macs = flip map xs $ \(i, o) -> leaf i "mac" (String . pack . show . NI.mac $ o) Up
    return $ root baseLeaf $
        [ root (leaf 0 "indexes" Zero Up) indexes 
        , root (leaf 1 "name" Zero Up) names 
        , root (leaf 2 "ipv4" Zero Up) ipv4s 
        , root (leaf 3 "ipv6" Zero Up) ipv6s 
        , root (leaf 4 "mac" Zero Up) macs 
        ]




