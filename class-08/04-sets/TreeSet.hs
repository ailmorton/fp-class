module TreeSet (Set, empty, insert, member) where

import AbstractSet

newtype Set t = Set (Tree t)

data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving (Eq,Show)

emptyTree = Empty

addElem Empty x = Node (emptyTree) x (emptyTree)
addElem (Node t1 a t2) x = if (compare a x == GT) then (Node (addElem t1 x) a t2 ) else (Node t1 a (addElem t2 x))

treeSize Empty  = 0
treeSize (Node t1 a t2) = 1 + (treeSize t1) + (treeSize t2)

treeMember Empty _ = False
treeMember (Node t1 a t2) x = if (compare a x == EQ) then True else (treeMember t1 x) || (treeMember t2 x)

instance AbstractSet Set where
  empty = Set (emptyTree);
	size (Set t) = treeSize t
  member x (Set t) = treeMember t x
  insert x (Set t) = if member x (Set t) then (Set t) else (Set (addElem t x))

