module AbstractSet where

class AbstractSet a where
  empty :: a t ;
	size :: a t -> Int 
  insert :: Ord t => t -> a t -> a t ;
	member :: Ord t => t -> a t -> Bool


