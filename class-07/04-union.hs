{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и сумму различных чисел, встречающихся
  в заданных текстовых файлах.
-}

import System.Environment
import Data.List
import Data.Char
import qualified Data.IntSet as Set

readNumFile :: FilePath -> IO [Int]
readNumFile fname = do
  content <- readFile fname ;
	return $ map read $ Data.List.concatMap words $ lines content 

solve :: [[Int]] -> (Int, [Int])
solve (l:ls) 
	| ls==[] = (Set.size s, Set.elems s)
	| otherwise = (Set.size un,Set.elems un)
  where 
		s = Set.fromList l 
		solvs = solve ls
		un = Set.union (Set.fromList (snd solvs)) s

solve' x = (cnt, sum ints)
  where (cnt,ints) = solve x

main = getArgs >>= mapM readNumFile >>= print.solve'
