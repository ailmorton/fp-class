-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms a = (div a 3600,mod (div a 60) 60,mod a 60)

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h*3600+m*60+s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h, m, s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt (sqr (x1-x2) + sqr (y1-y2) )
  where 
    sqr x = x*x

triangle :: (Double, Double, Double) -> (Double, Double)
triangle (a,b,c) = (p, s)
  where
    p = a+b+c
    pp = p/2
    s = sqrt $ pp*(pp-a)*(pp-b)*(pp-c)

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs) 
  | mod x 2 == 0 && x /=0 = 1+nEven(xs)
  | otherwise = nEven(xs) 

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems []  = []
doubleElems (x:xs) = x*2 : doubleElems xs

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs)
  | mod x 2 /= 0 = x : fltOdd xs
  | otherwise = fltOdd xs

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
remNeg :: (Num a, Ord a) => [a] -> [a]
remNeg [] = []
remNeg (x:xs)
  | x<0 = remNeg xs
  | otherwise = x : remNeg xs

-- б) увеличить элементы с чётными значениями в два раза;
doubleEven :: Integral a => [a] -> [a]
doubleEven []  = []
doubleEven (x:xs)
  | mod x 2 == 0 && x /=0  = x*2 : doubleEven xs
  | otherwise = x : doubleEven xs

-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).
swapPairs :: [a] -> [a]
swapPairs [] = []
swapPairs [_] = []
swapPairs (a:b:xs) = b:a: swapPairs xs


-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = x+y : combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
zip' :: [a] -> [b] -> [(a, b)]
zip' [] ys = []
zip' xs [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
n_list :: (Num t, Ord t) => t -> [t]
n_list 1 = [1]
n_list n
  | n>0 = n : n_list (n-1)
  | otherwise = error "n>0" 

-- б) в порядке возрастания.
n_list_ordered :: (Num t, Ord t) => t -> [t]
n_list_ordered n
  | n>0 = n_ordered 1 n
  | otherwise = error "n>0"
  where
    n_ordered a n
      | a==n = [n]
      | otherwise = a : n_ordered (a+1) n

-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
insert_everywhere :: a -> [a] -> [a]
insert_everywhere x [] = []
insert_everywhere x [a] = [a]
insert_everywhere x (y:ys) = y : x : insert_everywhere x ys

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).

split_first_same :: Eq a => [a] -> ([a], [a])
split_first_same [] = ([],[])
split_first_same (first:list) = get_first_same first (first:list)
  where
  get_first_same x [] = ([],[])
  get_first_same x (y:ys)
    | x==y = ((x : fst plist),(snd plist))
    | otherwise = ([],(y:ys))
    where 
      plist = get_first_same x ys

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
-- Получает i-й элемент списка
get_elem :: [a] -> Int -> a
get_elem list n = get_i 1 list  
  where
  get_i cur_n [] = error "no such element"
  get_i cur_n (y:ys)
    | cur_n==n = y
    | otherwise =  get_i (cur_n+1) ys

-- б) Eq a => [a] -> a -> Bool
-- Проверяет наличие элемента в списке
includes :: Eq a => [a] -> a -> Bool
includes list x = find_elem list  
  where
  find_elem [] = False
  find_elem (y:ys)
    | y==x = True
    | otherwise =  find_elem ys
-- в) [a] -> Int -> [a]
-- Удаляет i-й элемент
remove_i :: [a] -> Int -> [a]
remove_i list n = rem_i 1 list  
  where
  rem_i cur_n [] = []
  rem_i cur_n (y:ys)
    | cur_n==n = ys
    | otherwise = y : rem_i (cur_n+1) ys
-- г) a -> Int -> [a]
-- Создает список из n элементов x, где x-первый аргумент, n-второй
repeatN :: a -> Int -> [a]
repeatN a 0 = []
repeatN a n 
  |n>0 = a : repeatN a (n-1)
  |otherwise = error "2-nd argument>=0"

-- д) [a] -> [a] -> [a]
--Объединяет два списка
unite :: [a] -> [a] -> [a]
unite [] [] = []
unite xs [] = xs
unite [] xs = xs
unite (x:xs) ys = x : unite xs ys
-- е) Eq a => [a] -> [[a]]
-- Разбивает список на список из двух подсписков: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные.
split_first_same_list :: Eq a => [a] -> [[a]]
split_first_same_list list = [fst p, snd p]
  where
  p = split_first_same list
-- ж) [a] -> [(Int, a)]
-- Задает нумерацию списка
set_numeration :: [a] -> [(Int, a)]
set_numeration l = set_num 1 l
  where
    set_num _ [] = []
    set_num n (x:xs) = (n,x) : set_num (n+1) xs
-- з) Eq a => [a] -> [a]
-- Удаляет первый элемент и все следующие за ним, совпадающие с первым.
remove_first_same :: Eq a => [a] -> [a]
remove_first_same [] = []
remove_first_same (first:list) = rem_first_same first (first:list)
  where
  rem_first_same x [] = []
  rem_first_same x (y:ys)
    | x==y = rem_first_same x ys
    | otherwise = (y:ys)
