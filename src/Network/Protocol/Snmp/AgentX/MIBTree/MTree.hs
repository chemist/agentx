{-# LANGUAGE OverloadedStrings #-}
module Network.Protocol.Snmp.AgentX.MIBTree.MTree where

import Network.Protocol.Snmp.AgentX.Packet (Context)
import Network.Protocol.Snmp.AgentX.MIBTree.Zipper
import Data.Monoid ((<>))
import Data.Maybe (fromJust)

data MTree a = Node Integer (MTree a) (MTree a)
             | Leaf Integer (Maybe Context) a (MTree a) 
             | Empty

instance Show a => Show (MTree a) where
    show f = unlines $ drawLevel f
      where
        drawLevel Empty = []
        drawLevel (Node i next link) = ("Node " <> show i <> " " ) : (drawSubtree next link)
        drawLevel (Leaf i c v next) = ("Leaf " <> show i <> " " <> show v <> " " <> show c) : (drawSubtree next Empty)
        
        drawSubtree next link = (shift "`- " " | " (drawLevel link)) <> drawLevel next
    
        shift first rest = zipWith (++) (first : repeat rest)

instance Zippers MTree where
    toZipper t = (t, [])

    attach t (_, bs) = (t, bs)

    goNext (Empty, _) = Nothing
    goNext (Node _ Empty _, _) = Nothing
    goNext (Leaf _ _ _ Empty, _) = Nothing
    goNext (Leaf i c v next     , bs) = Just (next, Next (Leaf i c v Empty     ):bs)
    goNext (Node i     next link, bs) = Just (next, Next (Node i     Empty link):bs)

    goLevel (Empty, _) = Nothing
    goLevel (Leaf{}, _) = Nothing
    goLevel (Node _ _ Empty, _) = Nothing
    goLevel (Node i next link, bs) = Just (link, Level (Node i next Empty):bs)

    goBack (_, []) = Nothing
    goBack (t, Next  (Leaf i c v Empty      ):bs) = Just (Leaf i c v t        , bs)
    goBack (t, Next  (Node i     Empty  link):bs) = Just (Node i     t    link, bs)
    goBack (t, Level (Node i     next  Empty):bs) = Just (Node i     next t   , bs)
    goBack _ = Nothing

    goUp (_, []) = Nothing
    goUp (t, Next  (Leaf i c v Empty      ):bs) = goUp (Leaf i c v t        , bs)
    goUp (t, Next  (Node i     Empty link ):bs) = goUp (Node i     t    link, bs)
    goUp (t, Level (Node i     next  Empty):bs) = Just (Node i     next t   , bs)
    goUp _ = Nothing

    top (t,[]) = (t,[])  
    top z = top (fromJust $ goBack z)

    oid (z, m) = foldl fun [gi z] m
      where 
        fun xs (Next{}) = xs
        fun xs (Level x) = gi x : xs
        gi :: MTree a -> Integer
        gi (Node i _ _) = i
        gi (Leaf i _ _ _) = i
        gi _ = error "oid"

    setCursor [] _ z = Just z
    setCursor ys mc z = walk ys (top z)
      where
      giz (Node i _ _  , _) = (i, Nothing)
      giz (Leaf i mc' _ _, _) = (i, mc')
      giz _ = error "goClosest: giz"
      walk [] t = Just t
      walk (x : []) t
        | (x, mc) == giz t = Just t
        | otherwise = goNext t >>= walk (x : []) 
      walk (x : xs) t 
        | x == fst (giz t) = goLevel t >>= walk xs 
        | otherwise = goNext  t >>= walk (x : xs) 

    cursor ((Node i   _ _), _) = Just (i, Nothing)
    cursor ((Leaf i c _ _), _) = Just (i, c)
    cursor (Empty       , _) = Nothing

    insert a Empty = a
    insert Empty a = a
    insert (Node i next link) x@(Node i1 next1 link1)
       | i == i1 = Node i (next `insert` next1) (link `insert` link1) 
       | otherwise = Node i (next `insert` x) link 
    insert (Node i next link) x@(Leaf{}) = Node i (next `insert` x) link 
    insert (Leaf i c v next) x = Leaf i c v (next `insert` x) 



