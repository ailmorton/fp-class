{-
  Все задачи в этом задании должны решаться исключительно с помощью свёрток.
  Явная рекурсия не допускается. Если в решении в качестве вспомогательной
  требуется стандартная функция обработки списков (помимо fold*, scan*), она
  также должна реализовываться свёрткой.

  Каждое решение должно сопровождаться тремя различными тестовыми примерами, которые при запуске
  возвращают True, например:

  f = undefined -- решение 
  f_test1 = f undefined == undefined -- тест 1
  f_test2 = f undefined == undefined -- тест 2
  f_test3 = f undefined == undefined -- тест 3
-}

{-
 1. Простейшие функции обработки списков
  a) Найти сумму чётных элементов списка с целочисленными элементами.
  b) Найти сумму и произведение элементов списка вещественных чисел.
  с) Найти среднее арифметическое элементов списка вещественных чисел (функцией length пользоваться нельзя,
     решение должно выполняться в один проход).
  d) Найти минимальный элемент списка.
  e) Найти наименьший нечётный элемент списка с целочисленными значениями (дополнительным параметром
     функции должно быть значение, возвращаемое по умолчанию).
-}

f1a :: [Integer] -> Integer
f1a = foldl (\z x -> if even x then z+x else z) 0

f1b :: [Double] -> (Double, Double)
f1b = foldl (\(s,p) x -> (s+x,p*x)) (0.0,1.0)

f1c :: [Double] -> Double
f1c l = (fst sum_cnt) / (snd sum_cnt) 
  where sum_cnt = foldl (\(s,c) x -> (s + x,c+1)) (0.0,0) l

f1d :: Ord a => [a] -> a
f1d [] = error "empty list"
f1d (a:xs) = foldl (\z x -> min z x) a xs 

max_int = maxBound::Int
f1e :: [Int] -> Int -> Int
f1e list def_elem = if (find_odd==max_int) then def_elem else find_odd
  where find_odd = foldl (\a b -> if odd b then min a b else a) max_int list
{-
 2. Свёртки, формирующие списки
  a) Сформировать список, содержащий каждый второй элемент исходного.
  b) Сформировать список, содержащий первые n элементов исходного.
  c) Сформировать список, содержащий последние n элементов исходного.
  d) Сформировать список, содержащий все элементы исходного списка, большие левого соседа.
  e) Сформировать список, содержащий все локальные минимумы исходного списка.
  f) Дана строка, содержащая слова, разделённые одним или несколькими пробелами. Сформировать
     список слов этой строки.
  g) Разбить список на непересекающиеся подсписки длиной n элементов.
  h) Разбить список на подсписки длиной n элементов с перекрывающейся частью в k элементов (k < n).
  k) Сформировать список, содержащий все начальные элементы списка, удовлетворяющие заданному предикату.
  l) Повторить каждый элемент списка заданное количество раз.
  m) Удалить из списка повторяющиеся подряд идущие элементы.
  n) Даны два списка одинаковой длины. Сформировать список, состоящий из результатов применения
     заданной функции двух аргументов к соответствующим элементам исходных списков.
-}

{-
 3. Использование свёртки как носителя рекурсии (для запуска свёртки можно использовать список типа [1..n]).
  a) Найти сумму чисел от a до b.
  b) Найти сумму факториалов чисел от a до b (повторные вычисления факториалов не допускаются).
  с) Сформировать список из первых n чисел Фибоначчи.
  d) Пользуясь рядом Тейлора, вычислить значение синуса заданного числа x (использовать
     n слагаемых).
  e) Проверить, является ли заданное целое число простым.
-}

{-
 4. Решить задачу о поиске пути с максимальной суммой в треугольнике (см. лекцию 3) при условии,
   что необходимо дополнительно найти сам путь (к примеру, в виде закодированных направлений спуска:
   0 - влево, 1 - вправо). В решении допускается использование любых стандартных функций.
-}

{-
 5. Пусть числовые матрицы представлены списками строк. Реализовать следующие функции:
  1) транспонирование матрицы;
  2) сумма двух матриц;
  3) произведение двух матриц.
-}


{-
 6. Реализовать левую свёртку, пользуясь правой. Проанализировать поведение собственной реализации
  на бесконечных списках и сравнить его с поведением оригинальной foldl.
-}