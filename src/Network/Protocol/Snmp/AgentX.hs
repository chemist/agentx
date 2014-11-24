module Network.Protocol.Snmp.AgentX where

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Types
import Network.Protocol.Snmp.AgentX.Monads
import Network.Protocol.Snmp.AgentX.ATree
import Data.Tree
import Network.Info
import qualified Network.Info as NI
import Data.ByteString.Char8 (pack)
import Data.Fixed (div')
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Applicative
import qualified Data.Tree.Zipper as Zip
import Data.IORef
import Debug.Trace


fixmon :: IO ATree
fixmon = do
    io <- newIORef 0
    let b = (base [1,3,6,1,4,1,44729] "Fixmon")
    t <- generateTime
    n <- generateInterfaces
    i <- genSave io
    return $ root b [t, n, i]

generateTime :: IO ATree
generateTime = do
    t <- flip div' 1 <$> getPOSIXTime
    let time = leaf 0 "time" (TimeTicks (fromIntegral t)) (ReadOnly generateTime) 
    return time 

generateInterfaces :: IO ATree
generateInterfaces = do
    nx <- getNetworkInterfaces
    let baseLeaf = (leaf 1 "interfaces" Zero (ReadOnly generateInterfaces))
        xs = zip [0 .. fromIntegral $ length nx - 1] nx
        indexes = flip map xs $ \(i,_) -> leaf i "" (Integer . fromIntegral $ i) Fixed
        names = flip map xs $ \(i, o) -> leaf i "name" (String . pack . NI.name $ o) Fixed
        ipv4s = flip map xs $ \(i, o) -> leaf i "ipv4" (String . pack . show . NI.ipv4 $ o) Fixed
        ipv6s = flip map xs $ \(i, o) -> leaf i "ipv6" (String . pack . show . NI.ipv6 $ o) Fixed
        macs = flip map xs $ \(i, o) -> leaf i "mac" (String . pack . show . NI.mac $ o) Fixed
    return $ root baseLeaf $
        [ root (leaf 0 "indexes" Zero Fixed) indexes 
        , root (leaf 1 "name" Zero Fixed) names 
        , root (leaf 2 "ipv4" Zero Fixed) ipv4s 
        , root (leaf 3 "ipv6" Zero Fixed) ipv6s 
        , root (leaf 4 "mac" Zero Fixed) macs 
        ]

genSave :: IORef Integer -> IO ATree
genSave io = do
    x <- readIORef io
    return $ leaf 2 "ioref" (Integer $ fromIntegral x) (ReadWrite (genSave io) (saveIO io))

saveIO :: IORef Integer -> ATree -> IO ()
saveIO io (Node (Values _ _ (Integer v) _) _ ) = atomicWriteIORef io (fromIntegral v)
 
saveValue :: Value -> (ATree -> IO ()) -> IO ()
saveValue v f = f (Node (Values undefined undefined v undefined) undefined) 


