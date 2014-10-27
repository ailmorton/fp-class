{-
  Создать текстовый файл, содержащий случайные целые числа, разделённые пробелами
  и символами перевода строки. В командной строке должны задаваться следующие параметры:
  1) имя создаваемого файла;
  2) диапазон генерируемых случайных чисел: от и до;
  3) количество чисел в строке;
  4) количество строк в файле.
-}
import System.Environment
import System.Directory
import System.IO
import System.Random
import Data.Char


getRandStr nmin nmax m gen = concat $ map (++" ") $ map show $ take m $ (randomRs (nmin,nmax) gen :: [Int])

createFile n nmin nmax tn fname m
  | n> tn = do 
  	gen <- newStdGen ;
		appendFile fname $ (getRandStr nmin nmax m gen)++"\n" ;
	  createFile n nmin nmax (tn+1) fname m
  | otherwise = do
  	gen <- newStdGen ;
		appendFile fname $ (getRandStr nmin nmax m gen)++"\n"

main = do
  [fname, min_val, max_val, n_int, n_str] <- getArgs
  writeFile fname ""
  createFile (read n_str) (read min_val) (read max_val) 1 fname (read n_int) 
	nub_arr $ listArray (1,length xs) xs 
	print nub_arr

