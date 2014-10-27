{-
   Напишите программу обработки квадратных матриц (на массивах) со следующими возможностями:
   1) чтение матрицы из тестового файла;
   2) запись матрицы в текстовый файл;
   3) сумма матриц;
   4) произведение матриц.

  Задание на обработку может выглядеть, к примеру, следующим образом (здесь вычисляется матричное
  выражение (A + B) * C):

    LOAD a FROM matr1.txt
    LOAD b FROM matr2.txt
    LOAD c FROM matr3.txt
    CALC d AS PLUS a b
    CALC e AS MULT d c
    SAVE d TO matr4.txt

   Параметром командной строки должно быть имя файла со сценарием указанного или подобного ему вида.
-}
import Data.List
import Data.Array.IArray
import System.Environment

-- Печать матрицы на консоль
printMatr matr i j n
	| j<=n && i<=n = do
		putStr $ ( show $ matr!(i,j) )++" "
		printMatr matr i (j+1) n
	| j==(n+1) && i<=n = do
		putStrLn ""
		printMatr matr (i+1) 1 n
	| otherwise = putStrLn ""

writeM matr i j n fname
	| j<=n && i<=n = do
		appendFile fname $ ( show $ matr!(i,j) )++" "
		writeM matr i (j+1) n fname
	| j==(n+1) && i<=n = do
		appendFile fname "\n"
		writeM matr (i+1) 1 n fname
	| otherwise = appendFile fname "\n"

--2) запись матрицы в текстовый файл;
writeMatr matr fname = do
  writeFile fname $ (show (fst ( snd(bounds matr))))++"\n" ;
	writeM matr 1 1 (fst ( snd(bounds matr) )) fname

--3) сумма матриц;
matSum :: Array (Int,Int) Int -> Array (Int,Int) Int -> IO (Array (Int,Int) Int)
matSum x y = do
	return $ array resultBounds [ ((r, c), x!(r, c) + y!(r, c)) |  r <- range(lr,ur), c <- range(lc,uc) ]
	where
		bx@((lr,lc),(ur,uc)) = bounds x
		resultBounds
			| bx == bounds y = bx
			| otherwise = error "matSum: incompatible bounds"

--4) произведение матриц.
matMult :: Array (Int,Int) Int -> Array (Int,Int) Int -> IO (Array (Int,Int) Int)
matMult x y = do
	return $ array resultBounds [((i,j), Data.List.sum [x!(i,k) * y!(k,j) | k <- range (lc,uc)]) | i <- range (lr,ur), j <- range (lc,uc) ]
	where 
		bx@((lr,lc),(ur,uc)) = bounds x
		resultBounds
			| bx == bounds y = bx
			| otherwise = error "matMult: incompatible bounds"

--1) чтение матрицы из тестового файла; пример задания матрицы в файле matrix_example.txt
readMatr :: FilePath -> IO (Array (Int, Int) Int)
readMatr fname = do
  content <- readFile fname ;
  let (n:xs) = map read $ Data.List.concatMap words $ lines content ;
  return $ listArray ((1,1), (n,n)) xs 


main = do
  [fname1,fname2,fname_sumResult,fname_multResult] <- getArgs ;
  matr1 <- readMatr fname1;
  matr2 <- readMatr fname2;
	sumMatr <- matSum matr1 matr2;
	multMatr <- matMult matr1 matr2;
	writeMatr sumMatr fname_sumResult;
	writeMatr multMatr fname_multResult
