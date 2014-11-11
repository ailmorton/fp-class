import System.Environment
import System.IO
import System.Random


{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a 
	| (mod a 3)==0 = 0 
	| even a = a^2 
	| otherwise = a^3 

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF n x = foldr (\a z -> fmap reduce z) x [1..n]

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList = map (\(a,b) -> a^b)  

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe = foldr (\(a,b) z -> if (reduce a==b) then (Just a) else z) Nothing



toEither :: Integral a => [(a, a)]  -> Either String a
toEither l = if (f==k && (not (even k))) then Left "No even results" else Right f
	where 
		k = (fst (head l)) ^ (snd (head l))
		f = foldr (\(a,b) z-> if even (a^b) then max z (a^b) else z) k l

-- воспользуйтесь в этой функции случайными числами

getRand gen k = head $ (randomRs (1,k) gen :: [Int])

rand k = do
  gen <- newStdGen ;
	return $ getRand gen k
	

toIO :: Integral a => [(a, a)]  -> IO a
toIO l = do
	r <- rand $ (length l) - 1
	return $ foldr (\(a,b) z -> max (a^b) z) ((fst (head l)) ^ (snd (head l))) ( take r l)
		

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs l = (head l, read (head(tail l)))

toPairs [] = []
toPairs [_] = []
toPairs l = (read (head l)::Int, read(head(tail l))::Int): toPairs (drop 2 l)

readData :: FilePath -> IO [(Int, Int)]
readData fname = do
	content <- readFile fname
	return $ toPairs $ words $ content

main = do
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  print $ reduceNF n (toEither ps)
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
-}
{-

*Main> :main testFile1.txt 1
Right 0
1953125

*Main> :main testFile1.txt 5
Right 0
1

*Main> :main testFile2.txt 6
Left "No even results"
0

*Main> :main testFile3.txt 3
Right 4294967296
8931748751512963525


-}

