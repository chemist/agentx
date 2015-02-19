{-# LANGUAGE StandaloneDeriving #-}
module Network.Protocol.Snmp.AgentX.MIBTree.MIB where

import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Network.Protocol.Snmp (OID)
import Network.Protocol.Snmp.AgentX.Packet (Context)

type Parent = String
type Name   = String

data MIB m a = Object
    { oi :: OID
    , int :: Integer
    , parent :: Parent
    , name  :: Name
    , update :: Maybe (Update m a)
    }      | ObjectType
    { oi :: OID
    , int :: Integer
    , parent :: Parent
    , name :: Name
    , context :: Maybe Context
    , val :: a
    }

mkObject :: (Monad m, MonadIO m) => Integer -> Parent -> Name -> Maybe (Update m a) -> MIB m a
mkObject = Object [] 

mkObjectType :: Integer -> Parent -> Name -> Maybe Context -> a -> MIB m a
mkObjectType = ObjectType []

fillOid :: [MIB m a] -> [MIB m a]
fillOid [] = []
fillOid (ObjectType o i p n v u : xs) 
  | o == [] = ObjectType [i] i p n v u : mkOid' [(p, []), (n, [i])] xs
  | otherwise = ObjectType o i p n v u : mkOid' [(p, []), (n, o)] xs
  where
    mkOid' :: [(Parent, OID)] -> [MIB m a] -> [MIB m a]
    mkOid' _ [] = []
    mkOid' base (y:ys) =
        let Just prev = lookup (parent y) base
            newbase = (name y, prev <> [int y]) : base
        in addOid prev y : mkOid' newbase ys
    addOid :: OID -> MIB m a -> MIB m a
    addOid o' (Object _ i' p' n' u') = Object (o' <> [i']) i' p' n' u'
    addOid o' (ObjectType _ i' p' n' v' u') = ObjectType (o' <> [i']) i' p' n' v' u'
fillOid (Object o i p n u : xs) 
  | o == [] = Object [i] i p n u :  mkOid' [(p, []), (n, [i])] xs
  | otherwise = Object o i p n u : mkOid' [(p, []), (n, o)] xs
  where
    mkOid' :: [(Parent, OID)] -> [MIB m a] -> [MIB m a]
    mkOid' _ [] = []
    mkOid' base (y:ys) =
        let Just prev = lookup (parent y) base
            newbase = (name y, prev <> [int y]) : base
        in addOid prev y : mkOid' newbase ys
    addOid :: OID -> MIB m a -> MIB m a
    addOid o' (Object _ i' p' n' u') = Object (o' <> [i']) i' p' n' u'
    addOid o' (ObjectType _ i' p' n' v' u') = ObjectType (o' <> [i']) i' p' n' v' u'


data Update m a = Update { unUpdate :: m [MIB m a]}

deriving instance Show a => Show (MIB m a)

instance Show a => Show (Update m a) where
    show _ = "Update Subtree Fun"
