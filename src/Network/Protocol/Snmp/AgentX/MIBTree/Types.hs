{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Network.Protocol.Snmp.AgentX.MIBTree.Types 
( PVal(..)
, rsValue
, rwValue
, rdValue
, Update(..)
, Module(..)
, mkModule
, MIB(..)
, mkObject
, mkObjectType
, isObjectType
, Parent
, Name
, unregister
, ou
, moduleOID
, MIBTree
, buildTree
, register
, isWritable
, zipper
, IUpdate
, IValue
, ContextedValue(..)
)
where

import Control.Monad.State.Strict 
import Control.Concurrent.MVar
import Data.Monoid ((<>))
import Data.Foldable (foldMap)
import Data.Label

import Network.Protocol.Snmp.AgentX.MIBTree.Tree 
import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Packet (Context, CommitError, TestError, UndoError)

-- | Wrapper for value
data PVal = Read 
            { readAIO        :: IO Value  
            }
          | ReadWrite 
            { readAIO        :: IO Value
            , commitSetAIO   :: Value -> IO CommitError
            , testSetAIO     :: Value -> IO TestError
            , undoSetAIO     :: Value -> IO UndoError
            }

-- | Update, for rebuild oid tree in runtime
newtype Update = Update { unUpdate :: forall  m . (Monad m, MonadIO m, Functor m) =>  m [MIB] }

type IValue = ContextedValue PVal  
type IUpdate = ContextedValue Update 

instance Show PVal where
    show Read{} = "Read Value"
    show ReadWrite{} = "ReadWrite Value"

type Parent = String
type Name   = String

-- | MIB describe objects and object-types in internal tree with data.
data MIB = Object
    { oi :: OID -- ^ accessor for OID
    , int :: Integer
    , parent :: Parent
    , name  :: Name
    , update :: Maybe Update 
    }      | ObjectType
    { oi :: OID
    , int :: Integer
    , parent :: Parent
    , name :: Name
    , context :: Maybe Context -- ^ accessor for Maybe Context
    , val :: PVal  -- ^ accessor for PVal
    }

deriving instance Show MIB 

instance Show Update  where
    show _ = "Update Subtree Fun"

newtype ContextedValue a = Contexted { unContext :: (Integer, Maybe Context, Maybe a) }

instance Contexted (ContextedValue a) where
    index (Contexted (i, _, _)) = i
    context (Contexted (_, c, _)) = c
    withValue (Contexted (_, _, Just _)) = True
    withValue _ = False

instance Show a => Show (ContextedValue a) where
    show (Contexted (_, Nothing, Nothing)) = "- node -"
    show (Contexted (_, Nothing, Just v)) = "- leaf " <> show v
    show (Contexted (_, Just c, Just v)) = "- contexted leaf " <> show c <> show v
    show _ = "bad node"

-- | internal state for build agentx submodule
data Module = Module
  { _zipper        :: Zipper Tree IValue 
  , _ou            :: Zipper Tree IUpdate
  , _moduleOID     :: OID
  , _register      :: MVar [MIB]
  , _unregister    :: MVar [MIB]
  } 

mkLabel ''Module

instance Show Module where
    show (Module z ou' _ _ _) = show z ++ "\n" ++ show ou'

-- | MIBTree, state transformer, with Module under ground
type MIBTree = StateT Module  

-- | Constructor for Module
--
--
mkModule :: (Monad m, MonadIO m, Functor m) => 
    OID -- ^ base module OID
  -> [MIB] -- ^ all MIB for create module
  -> m Module 
mkModule moduleOid mibs = do
    r <- liftIO $ newEmptyMVar
    unr <- liftIO $ newEmptyMVar
    return $ Module (toZipper . fst . buildTree $ mibs) (toZipper . snd . buildTree $ mibs) moduleOid r unr

buildTree :: [MIB] -> (Tree IValue, Tree IUpdate)
buildTree ms = foldMap singleton $ fillOid ms
  where
    singleton :: MIB -> (Tree IValue , Tree IUpdate)
    singleton m = singleton' (oi m, m)
      where
        singleton' :: (OID, MIB) -> (Tree IValue, Tree IUpdate)
        singleton' ([],  _) = (Empty, Empty)
        singleton' ([_], Object _ i _ _ Nothing) = (Node (zero i) Empty Empty, Empty )
        singleton' ([_], Object _ i _ _ u@_) = (Node (zero i) Empty Empty, Node (toC i Nothing u) Empty Empty )
        singleton' ([_], ObjectType _ i _ _ c v) = (Node (toC i c (Just v)) Empty Empty, Empty)
        singleton' ((i:xs), obj@(Object _ _ _ _ Nothing)) = (Node (zero i) Empty (fst $ singleton' (xs, obj)), Empty)
        singleton' ((i:xs), obj@(Object _ _ _ _ _)) = (Node (zero i) Empty (fst $ singleton' (xs, obj)), Node (zero i) Empty (snd $ singleton' (xs, obj)))
        singleton' ((i:xs), obj@(ObjectType{})) = (Node (zero i) Empty (fst $ singleton' (xs, obj)), Empty)

-- | check MIB subtype
isObjectType :: MIB -> Bool
isObjectType (ObjectType{}) = True
isObjectType _ = False

-- | Constructor for MIB, create Object in mib tree
mkObject :: Integer -- ^ OID number for this object
  -> Parent -- ^ parent name for this object
  -> Name  -- ^ name for this object
  -> Maybe Update  -- ^ Just Update if you need dynamic module
  -> MIB -- ^ created MIB
mkObject = Object [] 

-- | Constructor for MIB, create Object-Type in mib tree
mkObjectType :: Integer -- ^ OID number for this object
  -> Parent  -- ^ parent
  -> Name -- ^ name
  -> Maybe Context -- ^ context
  -> PVal -- ^ value
  -> MIB -- ^ created MIB 
mkObjectType = ObjectType []

fillOid :: [MIB ] -> [MIB ]
fillOid [] = []
fillOid (ObjectType o i p n v u : xs) 
  | o == [] = ObjectType [i] i p n v u : mkOid' [(p, []), (n, [i])] xs
  | otherwise = ObjectType o i p n v u : mkOid' [(p, []), (n, o)] xs
  where
    mkOid' :: [(Parent, OID)] -> [MIB ] -> [MIB ]
    mkOid' _ [] = []
    mkOid' base (y:ys) =
        let Just prev = lookup (parent y) base
            newbase = (name y, prev <> [int y]) : base
        in addOid prev y : mkOid' newbase ys
    addOid :: OID -> MIB -> MIB 
    addOid o' (Object _ i' p' n' u') = Object (o' <> [i']) i' p' n' u'
    addOid o' (ObjectType _ i' p' n' v' u') = ObjectType (o' <> [i']) i' p' n' v' u'
fillOid (Object o i p n u : xs) 
  | o == [] = Object [i] i p n u :  mkOid' [(p, []), (n, [i])] xs
  | otherwise = Object o i p n u : mkOid' [(p, []), (n, o)] xs
  where
    mkOid' :: [(Parent, OID)] -> [MIB ] -> [MIB ]
    mkOid' _ [] = []
    mkOid' base (y:ys) =
        let Just prev = lookup (parent y) base
            newbase = (name y, prev <> [int y]) : base
        in addOid prev y : mkOid' newbase ys
    addOid :: OID -> MIB  -> MIB  
    addOid o' (Object _ i' p' n' u') = Object (o' <> [i']) i' p' n' u'
    addOid o' (ObjectType _ i' p' n' v' u') = ObjectType (o' <> [i']) i' p' n' v' u'

-- | PVal constructor for read only value
rsValue :: Value -> PVal 
rsValue v = Read $ return v

-- | PVal constructor for read only IO Value 
rdValue :: IO Value -> PVal 
rdValue = Read   

-- | PVal constructor for read write value
rwValue :: IO Value -> (Value -> IO CommitError) -> (Value -> IO TestError) -> (Value -> IO UndoError) -> PVal 
rwValue = ReadWrite

-- | check for PVal
isWritable :: PVal -> Bool
isWritable ReadWrite{} = True
isWritable _ = False

toC :: Integer -> Maybe Context -> Maybe a -> ContextedValue a
toC i mc mv = Contexted (i, mc, mv)

zero :: Integer -> ContextedValue a
zero i = Contexted (i, Nothing, Nothing)


