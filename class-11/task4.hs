import Data.STRef
import Control.Monad.ST
import Control.Monad


sort_st :: [Integer] -> ST s [Integer]
sort_st list = do
	res <- newSTRef list
	modifySTRef res hoare_sort
	readSTRef res

sort' list = runST (sort_st list)


hoare_sort [] = []
hoare_sort (a:xs) = (hoare_sort l1)++[a]++(hoare_sort l2)
	where (l1,l2) = partition xs a


partition list x = foldr (\a (l1,l2) -> if (a<x) then (a:l1,l2) else (l1,a:l2)) ([],[]) list
