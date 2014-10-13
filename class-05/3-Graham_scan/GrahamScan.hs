{-# LANGUAGE EmptyDataDecls #-}

module GrahamScan where
import Data.List

-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.
type X = Double
type Y = Double
data Point = Point X Y deriving (Show, Eq)
  
{-
  2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
  заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
  может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
  этих трёх возможностей определить специальный тип Direction.
-}

data Direction = Rght | Lft | OnLine deriving (Show,Eq)

{-
  3. Определить функцию, которая принимает список точек и вычисляет список направлений поворотов
  для каждых трёх последовательных точек. Например, для списка точек [a, b, c, d, e] она возвращает
  список поворотов для точек [a, b, c], [b, c, d] и [c, d, e]. При написании этой функции рекомендуется
  определить несколько вспомогательных функций.
-}

triples [] = []
triples [_] = []
triples [_,_] = []
triples (x:xs) = take 3 (x:xs) : triples xs

direction [Point x0 y0,Point x1 y1,Point x2 y2] 
  | abs sa < 0.001 = OnLine 
  | sa > 0 = Lft
  | otherwise = Rght
  where sa = (x1-x0) * (y2-y0) - (x2-x0) * (y1-y0);

directions :: [Point] -> [Direction]
directions l = map direction $ triples l

{-
  4. Пользуясь решениями предыдущих упражнений, реализовать алгоритм Грэхема нахождения выпуклой
  оболочки множества точек на вещественной плоскости. Описание алгоритма можно взять в английском
  (Graham scan) или русском разделах Википедии. Там же можно разобраться с тем, что именно называют
  выпуклой оболочкой (convex hull). Визуализация порядка работы алгоритма имеется на Youtube:
  http://www.youtube.com/watch?v=BTgjXwhoMuI
-}

angle (Point x y) = acos (x/ sqrt (x*x +y*y))

cmpAngle p1 p2
  | ap1 > ap2 = GT  
  | ap1 < ap2 = LT
  | otherwise = EQ
  where ap1 = angle p1 
        ap2 = angle p2 

f [] pi = []
f [a] pi = [a]
f (x:(x1:xs)) pi 
  | dir /= Lft = f (x1:xs) pi
  | otherwise = (x:(x1:xs))
  where dir = direction [x1,x,pi]

f1 list1 [] = list1
f1 list1 (x:list2) = f1 (x:(f list1 x)) list2

graham_scan :: [Point] -> [Point]
graham_scan list = f1 l2 l1
  where (a:(b:l1)) = sortBy cmpAngle list
        l2 = [b,a]

{-
  5. Приведите несколько примеров работы функции graham_scan.
-}

graham_scan_test = graham_scan [Point 2 1, Point 1 2, Point 2 2, Point 3 3] == [Point 1.0 2.0,Point 3.0 3.0,Point 2.0 1.0]
