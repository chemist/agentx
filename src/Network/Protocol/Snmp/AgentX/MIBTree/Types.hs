{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.Protocol.Snmp.AgentX.MIBTree.Types where

import Control.Monad.State.Strict hiding (gets, modify)
-- import Control.Concurrent.MVar
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Label

import Data.Map.Strict (Map)
import Network.Protocol.Snmp (Value(..), OID)
import Network.Protocol.Snmp.AgentX.Packet (Context, CommitError, TestError, UndoError)

type CValue = Map Context Value

data PVal m = Read 
            { readAIO        :: m CValue }
          | ReadWrite 
            { readAIO        :: m CValue
            , commitSetAIO   :: Maybe Context -> Value -> m CommitError
            , testSetAIO     :: Maybe Context -> Value -> m TestError
            , undoSetAIO     :: Maybe Context -> Value -> m UndoError
            }

instance (Monad m, MonadIO m) => Show (PVal m) where
    show Read{} = "Read Value"
    show ReadWrite{} = "ReadWrite Value"

cursor :: Zipper a -> Maybe Integer
cursor ((Node i _ _), _) = Just i
cursor ((Leaf i _ _), _) = Just i
cursor (Empty       , _) = Nothing

data MTree a = Node Integer   (MTree a) (MTree a)
             | Leaf Integer a (MTree a) 
             | Empty

testTree :: MTree String
testTree = Node 0 Empty 
  (Node 11 
    (Leaf 12 "1 level" Empty) 
    (Node 21 
      (Leaf 22 "2 level" 
      (Leaf 23 "2 level" 
      (Leaf 24 "2 level" 
      Empty)))
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
goNext (Leaf _ _ Empty, _) = Nothing
goNext (Leaf i v next, bs) = Just (next, Next (Leaf i v Empty):bs)
goNext (Node i next link, bs) = Just (next, Next (Node i Empty link):bs)

goLevel :: Zipper a -> Maybe (Zipper a)
goLevel (Empty, _) = Nothing
goLevel (Leaf{}, _) = Nothing
goLevel (Node _ _ Empty, _) = Nothing
goLevel (Node i next link, bs) = Just (link, Level (Node i next Empty):bs)

goBack :: Zipper a -> Maybe (Zipper a)
goBack (_, []) = Nothing
goBack (t, Next (Leaf i v Empty):bs) = Just (Leaf i v t, bs)
goBack (t, Next (Node i Empty link):bs) = Just (Node i t link, bs)
goBack (t, Level (Node i next Empty):bs) = Just (Node i next t, bs)
goBack _ = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (_, []) = Nothing
goUp (t, Next (Leaf i v Empty):bs) = goUp (Leaf i v t, bs)
goUp (t, Next (Node i Empty link):bs) = goUp (Node i t link, bs)
goUp (t, Level (Node i next Empty):bs) = Just (Node i next t, bs)
goUp _ = Nothing

top :: Zipper a -> Zipper a
top (t,[]) = (t,[])  
top z = top (fromJust $ goBack z)

instance Show a => Show (MTree a) where
    show f = unlines $ drawLevel f
      where
        drawLevel Empty = []
        drawLevel (Node i next link) = ("Object " <> show i <> " " ) : (drawSubtree next link)
        drawLevel (Leaf i v next) = ("ObjectType " <> show i <> " " <> show v <> " " ) : (drawSubtree next Empty)
        
        drawSubtree next link = (shift "`- " " | " (drawLevel link)) <> drawLevel next
    
        shift first rest = zipWith (++) (first : repeat rest)



