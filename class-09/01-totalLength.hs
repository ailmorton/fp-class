import System.Environment

{-
  Написать функцию, которая по заданному списку строк возвращает сумму длин всех строк.
-}

totalLength :: [String] -> Int
totalLength list = sum $ map length list

{-
  Написать функцию, которая по заданному символу и целому числу n строит список строк,
  содержащих 1, 2, ..., n повторений символа. Функция должна возвращать Nothing, если n=0.
-}

build1 :: Char -> Int -> Maybe [String]
build1 c n
	| n==0 = Nothing
	| otherwise = Just (foldr (\x list ->(take x (repeat c)):list) [] [1..n])

{-
  Написать функцию, аналогичную по возможностям функции build1, но возвращающую при этом
  значение Either String [String], в котором значение слева должно свидетельствовать об
  одной из следующих особых ситуаций: 
  (*) n=0;
  (*) n > 100;
  (*) Роспотребнадзор запрещает создавать строки из символа 'x'.
-}

build2 :: Char -> Int -> Either String [String]
build2 c n 
	| n==0 = Left "n=0"
	| n>100 = Left "n>100"
	| c=='x' = Left "Роспотребнадзор запрещает создавать строки из символа 'x'"
	| otherwise = Right (foldr (\x list ->(take x (repeat c)):list) [] [1..n])



{-
  Параметрами командной строки являются имя файла, символ, целое число.
  1) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и
     вывести общую длину строк, переданных программе в качестве аргументов командной строки.
  2) Пользуясь функцией totalLength и возможностями IO, как функтора, подсчитать и вывести общую
     длину строк, содержащихся в заданном текстовом файле (результат readFile должен быть
     предварительно преобразован к списку строк).
  3) Пользуясь функцией totalLength, подсчитать общую длину строк для значений в контекстах,
     сформированных функциями build1 и build2 (в решении следует пользоваться возможностями
     Maybe и Either String как функторов).
-}

instance Functor (Either a) where
	fmap f (Right x) = Right (f x)
	fmap f (Left x) = Left x

-- В аргументах первым задается файл для считывания, затем символ и число n.
main = do
	tLen <- fmap totalLength getArgs
	putStrLn "Общая длина строк, переданных программе:"
	print tLen
	fname <- fmap head getArgs
	content <- fmap lines (readFile fname)
	putStrLn $ "Общая длина строк в файле "++fname++":"
	print $ totalLength content ;
	c <- fmap (\x -> head (head (tail x))) getArgs
	n <- fmap (\x -> read (head (drop 2 x))) getArgs
	putStrLn $ "Общая длина строк в контекстах :"
	print $ fmap totalLength (build1 c n)
	print $ fmap totalLength (build2 c n)
	
