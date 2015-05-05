{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Protocol.Snmp.AgentX.Handlers 
( route )
where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception (RecSelError, handle)
import qualified Data.Label as DL
import qualified Data.Map as Map
import Data.IORef
import Data.List (find)
import Data.Maybe

import Network.Protocol.Snmp.AgentX.MIBTree
import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.Packet
import Network.Protocol.Snmp.AgentX.Types

makePdu :: [Either TaggedError VarBind] -> SubAgent (Maybe PDU)
makePdu xs = do
  now <- uptime
  let (good, index, firstBad) = splitByError xs 
  case firstBad of
       Nothing -> return . Just $ Response now (Tagged NoAgentXError) index good
       Just err -> return . Just $ Response now err index good

splitByError :: [Either TaggedError a] -> ([a], Index, Maybe TaggedError)
splitByError xs = 
    case splitByError' xs of
         (xss, Nothing) -> (xss, minBound, Nothing)
         (xss, e) -> (xss, toEnum (1 + length xss), e) -- ? check position
    where
        splitByError' :: [Either TaggedError a] -> ([a], Maybe TaggedError)
        splitByError' [] = ([], Nothing)
        splitByError' (Left err : _) = ([], Just err)
        splitByError' (Right x : xs') = 
            let splitted = splitByError' xs'
            in (x : fst splitted, snd splitted)

-- | processing request, return response
route :: Packet -> SubAgent (Maybe Packet)
route packet = route' pdu' >>= return . fmap setPdu
    where
    pdu' = DL.get pdu packet
    setPdu = flip (DL.set pdu) packet
    transactionId = DL.get tid packet
    route' :: PDU -> SubAgent (Maybe PDU)
    route' (Get mcontext oids) = makePdu =<< getHandler oids mcontext
    route' (GetNext mcontext srange) = makePdu =<< getNextHandler mcontext srange 
    route' (GetBulk mcontext nonRepeaters maxRepeaters srange) = makePdu =<< getBulkHandler mcontext nonRepeaters maxRepeaters srange 
    route' (TestSet mcontext varBindList) = makePdu =<< testSetHandler mcontext varBindList transactionId
    route' CommitSet = do
        tr <- transactions <$> ask
        now <- uptime
        mtransaction <- Map.lookup transactionId <$> (liftIO . readIORef $ tr)
        liftIO $ atomicModifyIORef' tr $ \m ->  (Map.update (\x -> Just $ x { statusV = CleanupSetT }) transactionId m, ())
        case mtransaction of
             Just (Transaction mcontext varBindList TestSetT) -> do
                 result <- mapM (commit mcontext) varBindList
                 return $ maybe (Just $ Response now (Tagged NoCommitError) minBound []) 
                                (const $ Just $ Response now (Tagged CommitFailed) minBound [])
                                (find (\x -> snd x == CommitFailed) result)
             _ -> return . Just $ Response now (Tagged CommitFailed) minBound []
        where
          commit mcontext varbind' = do
              mib <- runMIBTree (findOne (DL.get vboid varbind') mcontext) 
              result <- liftIO $ commitSetAIO (val mib) (DL.get vbvalue varbind')
              return (varbind', result)
    route' CleanupSet = do
        tr <- transactions <$> ask
        maybeTransaction <-  Map.lookup transactionId <$> (liftIO . readIORef $ tr)
        let oidsList = map (DL.get vboid) $ fromMaybe [] (vblist `fmap` maybeTransaction)
        let mcontext = join $ tcontext `fmap` maybeTransaction
        liftIO . atomicModifyIORef' tr $  \m -> (Map.delete transactionId m, ())
        void $ runMIBTree (regWrapper (findMany oidsList mcontext))
        return Nothing
    
    route' _ = do
        liftIO $ print packet
        makePdu =<< return [Left (Tagged RequestDenied)]

uptime :: SubAgent SysUptime
uptime = do
    nowref <- sysuptime <$> ask
    liftIO . readIORef $ nowref

getHandler :: [OID] -> Maybe Context -> SubAgent [Either TaggedError VarBind]
getHandler xs mc = map Right <$> (liftIO . mapM mibToVarBind =<< runMIBTree (findMany xs mc))

getNextHandler :: Maybe Context -> [SearchRange] -> SubAgent [Either TaggedError VarBind]
getNextHandler mc xs = map Right <$> (liftIO . mapM mibToVarBind =<< runMIBTree (findManyNext xs mc))

getBulkHandler :: Maybe Context -> NonRepeaters -> MaxRepeaters -> [SearchRange] -> SubAgent [Either TaggedError VarBind]
getBulkHandler = undefined

testSetHandler :: Maybe Context -> [VarBind] -> TransactionID -> SubAgent [Either TaggedError VarBind]
testSetHandler mcontext varBindList transactionId = do
    tr <- transactions <$> ask
    result <- mapM testFun varBindList
    let (goods, _, _) = splitByError result
    liftIO . atomicModifyIORef' tr $ \x -> (Map.insert transactionId (Transaction mcontext goods TestSetT) x, ())
    return result
    where
      testFun v = do
          mib <- runMIBTree (findOne (DL.get vboid v) mcontext) 
          testResult <- liftIO $ handle (\(_ :: RecSelError) -> return NotWritable) $ testSetAIO (val mib) (DL.get vbvalue v)
          return $ if testResult == NoTestError
                      then Right v
                      else Left (Tagged testResult)


