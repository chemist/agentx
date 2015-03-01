{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.Protocol.Snmp.AgentX.MIBTree.Tree where

import Data.Maybe (fromJust)
import Data.Monoid 
-- import Control.Applicative
import Network.Protocol.Snmp.AgentX.Packet (Context)
import Network.Protocol.Snmp (OID)
import Prelude 
-- import Debug.Trace

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
    oid     :: Contexted a => Zipper b a -> OID
    cursor  :: Contexted a => Zipper b a -> Maybe (Integer, Maybe Context)
    setCursor :: Contexted a => OID -> Maybe Context -> Zipper b a -> Maybe (Zipper b a)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Functor, Eq)

class Contexted a where
    index :: a -> Integer
    context :: a -> Maybe Context
    withValue :: a -> Bool

instance Contexted a => Monoid (Tree a) where
    mempty = Empty
    mappend a Empty = a
    mappend Empty a = a
    mappend (Node v next link) x@(Node v1 next1 link1)
       | index v == index v1 && context v == context v1 = Node v (next <> next1) (link <> link1) 
       | otherwise = Node v (next <> x) link 


instance (Contexted a, Show a) => Show (Tree a) where
    show f = unlines $ drawLevel f
      where
        drawLevel Empty = []
        drawLevel (Node v next link) = (show (index v) <> " " <> show v <> " ") : (drawSubtree next link)

        drawSubtree next link = (shift "`- " " | " (drawLevel link)) <> drawLevel next
    
        shift first rest = zipWith (++) (first : repeat rest)

testTree :: Tree (Integer, Maybe String)
testTree = Node (0, Just "first") (Node (1, Just "second") Empty Empty) (Node (10, Just "third") Empty Empty)

instance Zippers Tree where
    toZipper t = (t, [])

    attach t (Empty, bs) = (t, bs)
    attach t (Node v next _, bs) = (Node v next t, bs)

    goNext (Empty, _) = Nothing
    goNext (Node _ Empty _, _) = Nothing
    goNext (Node x  next link, bs) = Just (next, Next (Node x   Empty link):bs)

    goLevel (Empty, _) = Nothing
    goLevel (Node _ _ Empty, _) = Nothing
    goLevel (Node x next link, bs) = Just (link, Level (Node x next Empty):bs)

    goBack (_, []) = Nothing
    goBack (t, Next  (Node x  Empty  link):bs) = Just (Node x  t    link, bs)
    goBack (t, Level (Node x  next  Empty):bs) = Just (Node x  next t   , bs)
    goBack _ = Nothing

    goUp (_, []) = Nothing
    goUp (t, Next  (Node x  Empty link ):bs) = goUp (Node x  t    link, bs)
    goUp (t, Level (Node x  next  Empty):bs) = Just (Node x  next t   , bs)
    goUp _ = Nothing

    top (t,[]) = (t,[])  
    top z = top (fromJust $ goBack z)

    oid (z, m) = foldl fun [gi z] m
      where 
        fun xs (Next{}) = xs
        fun xs (Level x) = gi x : xs
        gi :: Contexted a => Tree a -> Integer
        gi (Node x _ _) = index x
        gi _ = error "oid"

    setCursor [] _ z = Just (top z)
    setCursor ys c z = walk ys (top z)
      where
      giz (Node x _ _  , _) = (index x, context x)
      giz _ = error "setCursor: giz Empty Tree"
      walk [] t = Just t
      walk (x : []) t
        | (x, c) == giz t = Just t
        | otherwise = goNext t >>= walk (x : []) 
      walk (x : xs) t 
        | (x, c) == (giz t) = goLevel t >>= walk xs 
        | otherwise = goNext  t >>= walk (x : xs) 

    cursor ((Node v _ _), _) = Just (index v, context v)
    cursor (Empty       , _) = Nothing

hasLevel :: Zipper Tree a -> Bool
hasLevel (Node _ _ Empty, _) = False
hasLevel _ = True

hasNext :: Zipper Tree a -> Bool
hasNext (Node _ Empty _, _) = False
hasNext _ = True

goClosest :: Contexted a => OID -> Maybe Context -> Zipper Tree a -> Zipper Tree a
goClosest [] _ z = top z
goClosest ys c z = walk ys (top z)
  where
  giz (Node x _ _, _) = (index x, context x)
  giz _ = error "goClosest: giz Empty Tree"
  walk [] z' = z'
  walk (x : []) z'
    | (x,c) == giz z' = z'
    | otherwise = maybe z' (walk (x : [])) (goNext z')
  walk (x : xs) z'
    | (x, c) == giz z' = maybe z' (walk xs) (goLevel z')
    | otherwise = maybe z' (walk (x : xs)) (goNext z')
                      

