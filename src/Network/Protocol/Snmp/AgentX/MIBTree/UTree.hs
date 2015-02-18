module Network.Protocol.Snmp.AgentX.MIBTree.UTree where

import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Network.Protocol.Snmp.AgentX.MIBTree.Zipper

data UTree a = Node Integer (Maybe a) (UTree a) (UTree a)
             | Empty

instance Show a => Show (UTree a) where
    show f = unlines $ drawLevel f
      where
        drawLevel Empty = []
        drawLevel (Node i mv next link) = ("Node " <> show i <> " " <> maybe "" show mv <> " ") : (drawSubtree next link)

        drawSubtree next link = (shift "`- " " | " (drawLevel link)) <> drawLevel next
    
        shift first rest = zipWith (++) (first : repeat rest)

testTree :: UTree String
testTree = Node 0 (Just "first") (Node 1 (Just "second") Empty Empty) (Node 10 (Just "third") Empty Empty)


instance Zippers UTree where
    toZipper t = (t, [])
    attach t (_, bs) = (t, bs)

    goNext (Empty, _) = Nothing
    goNext (Node _ _ Empty _, _) = Nothing
    goNext (Node i mv  next link, bs) = Just (next, Next (Node i mv   Empty link):bs)

    goLevel (Empty, _) = Nothing
    goLevel (Node _ _ _ Empty, _) = Nothing
    goLevel (Node i mv next link, bs) = Just (link, Level (Node i mv next Empty):bs)

    goBack (_, []) = Nothing
    goBack (t, Next  (Node i  mv  Empty  link):bs) = Just (Node i mv   t    link, bs)
    goBack (t, Level (Node i  mv  next  Empty):bs) = Just (Node i mv   next t   , bs)
    goBack _ = Nothing

    goUp (_, []) = Nothing
    goUp (t, Next  (Node i  mv  Empty link ):bs) = goUp (Node i mv   t    link, bs)
    goUp (t, Level (Node i  mv  next  Empty):bs) = Just (Node i  mv  next t   , bs)
    goUp _ = Nothing

    top (t,[]) = (t,[])  
    top z = top (fromJust $ goBack z)

    oid (z, m) = foldl fun [gi z] m
      where 
        fun xs (Next{}) = xs
        fun xs (Level x) = gi x : xs
        gi :: UTree a -> Integer
        gi (Node i _ _ _) = i
        gi _ = error "oid"

    setCursor [] _ z = Just z
    setCursor ys mc z = walk ys (top z)
      where
      giz (Node i _ _ _  , _) = (i, Nothing)
      giz _ = error "goClosest: giz"
      walk [] t = Just t
      walk (x : []) t
        | (x, mc) == giz t = Just t
        | otherwise = goNext t >>= walk (x : []) 
      walk (x : xs) t 
        | x == fst (giz t) = goLevel t >>= walk xs 
        | otherwise = goNext  t >>= walk (x : xs) 

    cursor ((Node i _  _ _), _) = Just (i, Nothing)
    cursor (Empty       , _) = Nothing

    insert a Empty = a
    insert Empty a = a
    insert (Node i Nothing next link) x@(Node i1 _ next1 link1)
       | i == i1 = Node i Nothing (next `insert` next1) (link `insert` link1) 
       | otherwise = Node i Nothing (next `insert` x) link 
    insert (Node i (Just v) next link) x@(Node i1 _ next1 link1)
       | i == i1 = Node i (Just v) (next `insert` next1) (link `insert` link1) 
       | otherwise = Node i (Just v) (next `insert` x) link 



