{-
  Дан текстовый файл (его имя задано в параметрах командной строки), содержащий целые числа
  в диапазоне от 1 до 1000, разделённые пробелами и символами перевода строки. Определить
  количество различных чисел в нём, пользуясь для этого возможностями различных структур
  данных. 
-}

import Data.List
import qualified Data.Sequence as Seq
import qualified Data.IntSet as Set
import Data.Array.IArray
import System.Environment
import Control.Monad
import Data.Foldable

nub_set :: Set.IntSet -> Int
nub_set = Set.size

nub_list :: [Int] -> Int
nub_list = length.group.sort

nub_seq :: (Eq a) => Seq.Seq a -> Int
nub_seq s
	| Seq.fromList [] == s = 0
  | Data.Foldable.elem l s1 = nub_seq s1
  | otherwise = (nub_seq s1)+1
  where 
		(l Seq.:< _) = Seq.viewl s
		s1 = Seq.drop 1 s 

check_elems a n1 n2 x 
  | a ! n1 == x = True
	| n1 < n2 = check_elems a (n1+1) n2 x
  | otherwise = False 

count_diffrent a n len
  | n >= len = 1
  | fnd_el = count_diffrent a (n+1) len
  | otherwise = (count_diffrent a (n+1) len)+1
  where fnd_el = check_elems a (n+1) len (a ! n) 

nub_arr :: Array Int Int -> Int
nub_arr array = count_diffrent array 1  $ length $ indices array

main = do
  [fname] <- getArgs
  content <- readFile fname
  let xs = map read $ Data.List.concatMap words $ lines content
  let (n:results) = [
        nub_set $ Set.fromList xs,
        nub_list xs,
        nub_seq $ Seq.fromList xs,
        nub_arr $ listArray (1,length xs) xs ]
  Control.Monad.mapM_ print results
  when (Data.List.any (/= n) results) $ putStrLn "Результаты не совпадают!"
