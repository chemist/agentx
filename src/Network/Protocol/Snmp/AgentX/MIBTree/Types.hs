{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
module Network.Protocol.Snmp.AgentX.MIBTree.Types where

import Control.Monad.State.Strict hiding (gets, modify)
-- import Control.Concurrent.MVar
import Data.Maybe (fromJust, catMaybes)
import Data.Map.Strict (Map, fromList)
import Data.Monoid ((<>))
import Data.Label

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Packet (Context, CommitError, TestError, UndoError)

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

deriving instance Show a => Show (MIB m a)

mkObject :: (Monad m, MonadIO m) => Integer -> Parent -> Name -> Maybe (Update m a) -> MIB m a
mkObject = Object [] 

mkObjectType :: Integer -> Parent -> Name -> Maybe Context -> a -> MIB m a
mkObjectType = ObjectType []

prepareOid :: [MIB m a] -> [MIB m a]
prepareOid [] = []
prepareOid (ObjectType{} : _) = error "makeOid: first record cant be ObjectType"
prepareOid (Object o i p n u : xs) 
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

type MOU m a = Maybe (OID, Update m a)
type OU m a = Map OID (Update m a)
 
singleton :: (Monad m, MonadIO m) => MIB m a -> (MTree a, MOU m a)
singleton m = singleton' (oi m, oi m,  m)
  where
    singleton' :: (OID, OID, MIB m a) -> (MTree a, MOU m a)
    singleton' ([], _, _) = (Empty, Nothing)
    singleton' ([_], _, Object _ i _ _ Nothing) = (Node i Empty Empty, Nothing )
    singleton' ([_], o, Object _ i _ _ (Just u)) = (Node i Empty Empty, Just (o, u))
    singleton' ([_], _, ObjectType _ i _ _ c v) = (Leaf i c v Empty, Nothing)
    singleton' ((i:xs), o, obj@(Object _ _ _ _ Nothing)) = (Node i Empty (fst $ singleton' (xs, o, obj)), Nothing)
    singleton' ((i:xs), o, obj@(Object _ _ _ _ (Just u))) = (Node i Empty (fst $ singleton' (xs, o, obj)), Just (o, u))
    singleton' ((i:xs), o, obj@(ObjectType{})) = (Node i Empty (fst $ singleton' (xs, o, obj)), Nothing)

buildTree :: (Monad m, MonadIO m) => [MIB m a] -> (MTree a, OU m a)
buildTree xs = 
  let mib = map singleton $ prepareOid xs
  in (foldl1 insert . map fst $ mib, fromList $ catMaybes $ map snd mib)

insert :: MTree a -> MTree a -> MTree a
insert a Empty = a
insert Empty a = a
insert (Node i next link) x@(Node i1 next1 link1)
   | i == i1 = Node i (next `insert` next1) (link `insert` link1) 
   | otherwise = Node i (next `insert` x) link 
insert (Node i next link) x@(Leaf{}) = Node i (next `insert` x) link 
insert (Leaf i c v next) x = Leaf i c v (next `insert` x) 

data Update m a = Update { unUpdate :: m [MIB m a]}

instance Show a => Show (Update m a) where
    show _ = "Update Subtree Fun"

data PVal m = Read 
            { readAIO        :: m Value 
            }
          | ReadWrite 
            { readAIO        :: m Value
            , commitSetAIO   :: Value -> m CommitError
            , testSetAIO     :: Value -> m TestError
            , undoSetAIO     :: Value -> m UndoError
            }

readOnly :: (Monad m, MonadIO m) => Value -> PVal m
readOnly v = Read $ return v

instance (Monad m, MonadIO m) => Show (PVal m) where
    show Read{} = "Read Value"
    show ReadWrite{} = "ReadWrite Value"

cursor :: Zipper a -> Maybe (Integer, Maybe Context)
cursor ((Node i   _ _), _) = Just (i, Nothing)
cursor ((Leaf i c _ _), _) = Just (i, c)
cursor (Empty       , _) = Nothing

data MTree a = Node Integer (MTree a) (MTree a)
             | Leaf Integer (Maybe Context) a (MTree a) 
             | Empty

dc1 :: Maybe Context
dc1 = Just "context1"

dc :: Maybe Context
dc = Nothing

testTree :: MTree String
testTree = Node 0 Empty 
  (Node 11  
    (Leaf 12 dc "1 level" Empty) 
    (Node 21   
      (Leaf 22 dc "2 level" 
      (Leaf 22 dc1 "2 level" 
      (Node 23  
      (Leaf 24 dc1 "3 level"
      (Leaf 25 dc "3 level"
      (Leaf 26 dc "2 level" 
      Empty))) 
          (Leaf 30 dc "4 level" Empty)
      )))
        Empty)
  )

testZipper :: Zipper String
testZipper = (testTree, [])

data Move a = Next (MTree a) 
            | Level (MTree a)

instance Show a => Show (Move a) where
    show (Next x) = "\nNext " ++ show x
    show (Level x) = "\nLevel " ++ show x

type Moving a = [Move a]

type Zipper a = (MTree a, Moving a)

data Storage m a = Storage
  { _zipper        :: Zipper a
  , _ou            :: OU m a
  , _oid           :: OID
  , _moduleOID     :: OID
  } 

instance Show a => Show (Storage m a) where
    show (Storage z ou _ _) = show z ++ "\n" ++ show ou


mkLabel ''Storage

type ZipperM m a = StateT (Storage m a) m

toZipper :: MTree a -> Zipper a 
toZipper t = (t, [])

mkModule :: (Monad m, MonadIO m) => OID -> [MIB m a] -> Storage m a
mkModule o ms = 
  let (tr, u) = buildTree ms
  in Storage (toZipper tr) u [] o

attach :: MTree a -> Zipper a -> Zipper a 
attach t (_, bs) = (t, bs)

goNext :: Zipper a -> Maybe (Zipper a)
goNext (Empty, _) = Nothing
goNext (Node _ Empty _, _) = Nothing
goNext (Leaf _ _ _ Empty, _) = Nothing
goNext (Leaf i c v next     , bs) = Just (next, Next (Leaf i c v Empty     ):bs)
goNext (Node i     next link, bs) = Just (next, Next (Node i     Empty link):bs)

goLevel :: Zipper a -> Maybe (Zipper a)
goLevel (Empty, _) = Nothing
goLevel (Leaf{}, _) = Nothing
goLevel (Node _ _ Empty, _) = Nothing
goLevel (Node i next link, bs) = Just (link, Level (Node i next Empty):bs)

goBack :: Zipper a -> Maybe (Zipper a)
goBack (_, []) = Nothing
goBack (t, Next  (Leaf i c v Empty      ):bs) = Just (Leaf i c v t        , bs)
goBack (t, Next  (Node i     Empty  link):bs) = Just (Node i     t    link, bs)
goBack (t, Level (Node i     next  Empty):bs) = Just (Node i     next t   , bs)
goBack _ = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (_, []) = Nothing
goUp (t, Next  (Leaf i c v Empty      ):bs) = goUp (Leaf i c v t        , bs)
goUp (t, Next  (Node i     Empty link ):bs) = goUp (Node i     t    link, bs)
goUp (t, Level (Node i     next  Empty):bs) = Just (Node i     next t   , bs)
goUp _ = Nothing

top :: Zipper a -> Zipper a
top (t,[]) = (t,[])  
top z = top (fromJust $ goBack z)

instance Show a => Show (MTree a) where
    show f = unlines $ drawLevel f
      where
        drawLevel Empty = []
        drawLevel (Node i next link) = ("Node " <> show i <> " " ) : (drawSubtree next link)
        drawLevel (Leaf i c v next) = ("Leaf " <> show i <> " " <> show v <> " " <> show c) : (drawSubtree next Empty)
        
        drawSubtree next link = (shift "`- " " | " (drawLevel link)) <> drawLevel next
    
        shift first rest = zipWith (++) (first : repeat rest)

oidFromZipper :: Zipper a -> OID
oidFromZipper (z, m) = foldl fun [gi z] m
  where 
    fun :: OID -> Move a -> OID
    fun xs (Next{}) = xs
    fun xs (Level x) = gi x : xs
    gi :: MTree a -> Integer
    gi (Node i _ _) = i
    gi (Leaf i _ _ _) = i
    gi _ = error "oidsFromZipper"

c1 :: Maybe Context 
c1 = Just "context1"

setCursor :: OID -> Maybe Context -> Zipper a -> Maybe (Zipper a)
setCursor [] _ z = Just z
setCursor ys mc z = walk ys (top z)
  where
  giz :: Zipper a -> (Integer, Maybe Context)
  giz (Node i _ _  , _) = (i, Nothing)
  giz (Leaf i mc' _ _, _) = (i, mc')
  giz _ = error "goClosest: giz"
  walk :: OID -> Zipper a -> Maybe (Zipper a)
  walk [] t = Just t
  walk (x : []) t
    | (x, mc) == giz t = Just t
    | otherwise = goNext t >>= walk (x : []) 
  walk (x : xs) t 
    | x == fst (giz t) = goLevel t >>= walk xs 
    | otherwise = goNext  t >>= walk (x : xs) 

findMany :: [OID] -> Maybe Context -> [MIB m a]
findMany = undefined
