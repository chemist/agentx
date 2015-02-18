{-# LANGUAGE FlexibleContexts #-}
module Network.Protocol.Snmp.AgentX.MIBTree.Zipper where

import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.Packet (Context)

data Move b a = Next (b a)
              | Level (b a)

instance (Show a, Show (b a)) => Show (Move b a) where
    show (Next x) = "\nNext " ++ show x
    show (Level x) = "\nLevel " ++ show x

type Moving b a = [Move b a]

type Zipper b a = (b a, Moving b a)

class Zippers b where
    toZipper :: b a -> Zipper b a
    attach :: b a -> Zipper b a -> Zipper b a
    goNext :: Zipper b a -> Maybe (Zipper b a)
    goLevel :: Zipper b a -> Maybe (Zipper b a)
    goBack  :: Zipper b a -> Maybe (Zipper b a)
    goUp    :: Zipper b a -> Maybe (Zipper b a)
    top     :: Zipper b a -> Zipper b a
    oid     :: Zipper b a -> OID
    cursor  :: Zipper b a -> Maybe (Integer, Maybe Context)
    setCursor :: OID -> Maybe Context -> Zipper b a -> Maybe (Zipper b a)
    insert :: b a -> b a -> b a
    

