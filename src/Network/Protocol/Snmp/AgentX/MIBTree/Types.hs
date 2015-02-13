{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.Snmp.AgentX.MIBTree.Types where

import Control.Monad.State.Strict hiding (gets, modify)
-- import Control.Concurrent.MVar
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Label

import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Packet (Context, CommitError, TestError, UndoError)

data PVal m = Read 
            { readAIO        :: m Value }
          | ReadWrite 
            { readAIO        :: m Value
            , commitSetAIO   :: Value -> m CommitError
            , testSetAIO     :: Value -> m TestError
            , undoSetAIO     :: Value -> m UndoError
            }

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

data Storage a = Storage
  { _zipper        :: Zipper a
  , _oid           :: OID
  , _moduleOID     :: OID
  }

mkLabel ''Storage

type ZipperM m a = StateT (Storage a) m

toZipper :: MTree a -> Zipper a 
toZipper t = (t, [])

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

findOne :: OID -> Maybe Context -> Zipper a -> Maybe (Zipper a)
findOne [] _ z = Just z
findOne ys mc z = walk ys (top z)
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

findMany :: [OID] -> Maybe Context -> Zipper 