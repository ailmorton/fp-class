{-
Явная рекурсия в решениях хотя и допускается, но не приветствуется. Старайтесь обходиться стандартными
функциями, используя при этом создание функций «на лету». Пытайтесь максимально упростить уже написанные
решения, применяя подходящие функции из модуля Data.List и любых других модулей. Перед выполнением заданий
изучите примеры из лекции по функциям высшего порядка. 
-}
import Data.Char
import Numeric
import Data.List 
{-
 1. Простейшие задачи на применение функций map и filter.
 1.1 Преобразовать данный список целых чисел следующим образом:
  a) увеличить все его элементы в два раза;
  b) увеличить все его элементы с четными значениями в два раза;
  с) обнулить все его элементы с нечетными значениями;
  d) удалить из него элементы, большие заданного числа k;
  e) отфильтровать его, оставив в списке только отрицательные числа;
  f) удалить из него все положительные чётные числа.
-}

f11a :: Integral a => [a] -> [a]
f11a = map (*2) 

f11b :: [Integer] -> [Integer]
f11b = map f
  where
  f x 
    | even x = x*2
    | otherwise = x

f11c :: [Integer] -> [Integer]
f11c = map f
  where
  f x 
    | mod x 2 /= 0 = 0
    | otherwise = x

f11d :: Ord a => a -> [a] -> [a]
f11d k = filter (<=k)

f11e :: [Integer] -> [Integer]
f11e = filter (<0)

f11f :: [Integer] -> [Integer]
f11f = filter f
  where
  f x
    | x>0 && even x = False
    | otherwise = True
{-
 1.2 Дан список декартовых координат точек на плоскости (пар вещественных чисел).
     Преобразовать его следующим образом:
  a) отфильтровать список так, чтобы в нём остались точки из заданной координатной четверти;
  b) преобразовать декартовы координаты в полярные.
-}

f12a :: (Eq a, Num a, Num a2, Num a1, Ord a2, Ord a1) =>
     a -> [(a2, a1)] -> [(a2, a1)]
f12a k 
  | k==1 = filter (\x ->fst x>0 && snd x>0)
  | k==2 = filter (\x ->fst x<0 && snd x>0)
  | k==3 = filter (\x ->fst x<0 && snd x<0)
  | k==4 = filter (\x ->fst x>0 && snd x<0)
  | otherwise = error "неверная координатная четверть"


f12b :: [(Double, Double)] -> [(Double, Double)]
f12b = map (\x ->(r x, acos $ fst x / r x))
  where r x = sqrt $ fst x * fst x + snd x * snd x
{-
 1.3 Дан список слов.
  a) Преобразовать все слова к верхнему регистру.
  b) Извлечь из него подсписок слов заданной длины.
  c) Извлечь из него подсписок слов, начинающихся с заданной буквы.
-}

f13a :: [[Char]] -> [[Char]]
f13a = map wordToUpper
  where wordToUpper = map toUpper

f13b :: Int -> [[a]] -> [[a]]
f13b k = filter (len k) 
  where 
  len k list 
    | length list == k = True
    | otherwise = False

f13c :: Eq a => a -> [[a]] -> [[a]]
f13c c = filter (starts c) 
  where 
  starts c [] = False 
  starts c (x:xs) 
    | c==x = True
    | otherwise = False
{-
2. Формирование числовых последовательностей (iterate).
 a) Список натуральных чисел, начиная с 0.
 b) Список чётных чисел.
 c) Список элементов последовательности: a0=1, an=(1+an-1)/2.
 d) Список символов английского алфавита.
 e) Список строк, представляющих n-значные двоичные числа.
-}

nats :: [Integer]
nats = iterate (+1) 0

evens :: [Integer]
evens = iterate (+2) 2

f2c :: [Double]
f2c = iterate f 1
  where
    f x = (1 - x)/2

f2d :: [Char]
f2d = take 26 $ iterate f 'a'
  where
    f c = chr $ ord c + 1

f2e :: Integral a => a -> [String]
f2e n
  | n<=0 = error "n>0"
  | otherwise = take (2^(n-1)) $ iterate f $ showIntAtBase 2 intToDigit (2^(n-1)) ""
    where
      f str = showIntAtBase 2 intToDigit ((fst $ head $ readInt 2 (`elem` "01") digitToInt str)+1) "" 

{-
3. Группировка списков.
  a) Дан список символов. Сгруппировать подряд идущие символы по принципу: цифры — не цифры — ...
  b) Дан список пар вещественных чисел (координат точек на плоскости). Сгруппировать подряд идущие
     координаты точек, лежащие в одной координатной четверти.
  c) Дан список и ненулевое натуральное число n. Разбить список на подсписки длиной n каждый.
     Последний подсписок может содержать менее n элементов.
  d) Дан список и ненулевые натуральные числа n и m. Разбить список на перекрывающиеся подсписки
     длиной n элементов со сдвигом относительно предыдущего подсписка на m элементов.
  e) Дан список. Определить длину самого длинного подсписка, содержащего подряд идущие одинаковые элементы.
-}

f3a :: [Char] -> [[Char]]
f3a = groupBy f
  where f a b = (isDigit a) == (isDigit b)

f3b = groupBy f
  where f (x1,y1) (x2,y2) = (x1>0) == (x2>0) && (y1>0) == (y2>0)

f3c :: Int -> [a] -> [[a]]
f3c n list 
  | (length list)<=n = [list]
  | otherwise = take n list : f3c n (drop n list) 

f3d :: [a] -> Int -> Int -> [[a]]
f3d list n m
  | (length list)<=m = [list]
  | otherwise =  take n list : (f3d l1 n m) 
    where l1 = drop m list

-- Должно быть True
test_f3d = f3d [1..10] 4 2 == [[1,2,3,4],[3,4,5,6],[5,6,7,8],[7,8,9,10],[9,10]]

f3e :: Eq a => [a] -> Int
f3e l = maximum $ map length $ group l

{-
4. Разные задачи.
 a) Дан текст в виде строки символов, содержащий среди прочего числовые данные. Посчитать количество
    всех упоминающихся в тексте чисел.
 b) Найти сумму всех чисел Фибоначчи, удовлетворяющих заданному предикату, в указанном промежутке
    (например: все чётные от 1 до 106).
 c) Дана строка текста и число n. Сформировать список, содержащий n самых часто используемых
    в строке символов.
 d) Дан список чисел. Сформировать список локальных максимумов исходного списка. Локальным максимумом
    называется элемент, больший своих соседей.
 e) Дан список. Продублировать все его элементы.
-}

f4a :: [Char] -> Int
f4a l = length $ filter (\(x:xs) -> isDigit x) $ f3a l

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
f4b p n m = sum $ filter p $ dropWhile (<n) $ takeWhile (<=m) fibs

f4c :: Ord b => Int -> [b] -> [b]
f4c n list = map fst $ take n $ sortBy (\(a,b) (c,d) -> if b>d then LT else GT) $ map (\(x:xs) ->(x, length (x:xs))) $ group $ sort list

triples :: [a] -> [[a]]
triples list
  | (length list)<=3 = [list]
  | otherwise = take 3 list : (triples (drop 1 list)) 

min_int = minBound::Int
f4d l =map (\(a:b:xs) -> b) $ filter (\(a:b:c:xs) -> b>=a && b>=c) $ triples ((min_int : l) ++ [min_int])

f4e l = concat $ map (\x -> x : [x]) l

