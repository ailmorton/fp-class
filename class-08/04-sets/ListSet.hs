module ListSet (Set, empty, insert, member) where

import AbstractSet

newtype Set t = SetImpl [t]

instance AbstractSet Set where
  empty = SetImpl [] ;
	size (SetImpl xs) = length xs 
  member x (SetImpl xs) = foldr (\a z -> if (compare a x)==EQ then True else z) False xs
  insert x (SetImpl xs) = if member x (SetImpl xs) then (SetImpl xs) else (SetImpl (x:xs))

