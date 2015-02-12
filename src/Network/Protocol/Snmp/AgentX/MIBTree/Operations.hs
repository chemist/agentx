module Network.Protocol.Snmp.AgentX.MIBTree.Operations where

import Control.Monad.IO.Class (MonadIO, liftIO )
import Control.Monad.Trans.Class (lift)
import Control.Monad.State.Strict (modify, get)
import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.MIBTree.Types
-- import Data.Label.Monadic (gets, puts, modify)
import Control.Applicative hiding (empty)
-- import Control.Concurrent.MVar
import Control.Monad (void)


-- empty :: (Monad m, MonadIO m, Functor m) => OID -> Zipper m
-- empty oi = Zipper 

addNext :: (Monad m, MonadIO m, Functor m) => MTree (PVal m)  -> ZipperM m ()
addNext x = modify $ \st -> st { moving = Next x : moving st }

addLevel :: (Monad m, MonadIO m, Functor m) => MTree (PVal m)  -> ZipperM m ()
addLevel x = modify $ \st -> st { moving = Level x : moving st }

goNext ::(Monad m, MonadIO m, Functor m) =>  ZipperM m Bool
goNext = goNext' =<< zipper <$> get
  where
  goNext' Empty = return False
  goNext' (Node i next level) = do
      modify $ \st -> st { zipper = next }
      addNext (Node i Empty level)
      return True
  goNext' (Leaf i a next) = do
      modify $ \st -> st { zipper = next }
      addNext (Leaf i a Empty)
      return True

goLevel :: (Monad m, MonadIO m, Functor m) => ZipperM m Bool
goLevel = goLevel' =<< zipper <$> get
  where
  goLevel' Empty = return False
  goLevel' Leaf{} = return False
  goLevel' (Node i next level) = do
      modify $ \st -> st { zipper = level }
      addLevel (Node i next Empty)
      return True

goBack :: (Monad m, MonadIO m, Functor m) => ZipperM m Bool
goBack = goBack' =<< moving <$> get
  where
  goBack' [] = return False
  goBack' (Next (Node i Empty level) : bs) = do
      z <- zipper <$> get
      modify $ \st -> st { zipper = Node i z level
                        , moving = bs
                        }
      return True
  goBack' (Next (Leaf i a Empty) : bs) = do
      z <- zipper <$> get
      modify $ \st -> st { zipper = Leaf i a z
                        , moving = bs
                        }
      return True
  goBack' (Level (Node a next Empty) : bs) = do
      z <- zipper <$> get
      modify $ \st -> st { zipper = Node a next z 
                        , moving = bs
                        }
      return True
  goBack' _ = fail "bad case goBack'"


top :: (Monad m, MonadIO m, Functor m) => ZipperM m ()
top = top' =<< moving <$> get
  where
  top' [] = return ()
  top' _ = (void goBack) >> top


goUp :: (Monad m, MonadIO m, Functor m) => ZipperM m  Bool
goUp = goUp' =<< moving <$> get
  where
  goUp' [] = return False
  goUp' (Next (Leaf i a Empty) : bs) = do
      z <- zipper <$> get
      modify $ \st -> st { zipper = Leaf i a z
                        , moving = bs
                        }
      goUp
  goUp' (Next (Node a Empty level) : bs) = do
      z <- zipper <$> get
      modify $ \st -> st { zipper = Node a z level
                        , moving = bs
                        }
      goUp
  goUp' (Level (Node a next Empty) : bs) = do
      z <- zipper <$> get
      modify $ \st -> st { zipper = Node a next z 
                        , moving = bs
                        }
      return True
  goUp' _ = fail "bad case goUp'"

focus :: (Monad m, MonadIO m, Functor m) => ZipperM m ()
focus = do
    _o <- oid <$> get
    t <- zipper <$> get
    case t of
        Leaf _ (Read fun) _ -> do
            y <-  lift fun
            liftIO $ print y
             

isEmpty :: (Monad m, MonadIO m, Functor m) => ZipperM m Bool
isEmpty = isEmpty' . zipper <$> get 
  where
  isEmpty' :: MTree (PVal m)  -> Bool 
  isEmpty' Empty = True
  isEmpty' _     = False

isTop :: (Monad m, MonadIO m, Functor m) => ZipperM m Bool
isTop = null . moving <$> get 

