import Control.Monad 
{-
  Модифицируйте имеющуюся реализацию задачи о канатоходце (лекция 9) следующим образом:
  1) реализуйте загрузку входных данных из файла следующего вида:
       R 2
       L 3
       R -1
       B
       L 1
     и вычисление соответствующего им результата (в решении может пригодиться 
     функция foldr (<=<) return — проверьте её тип для получения подсказки);
  2) замените монаду Maybe на Either String так, чтобы в случае падения канатоходца
     можно было получить информацию о его причинах (нарушение баланса и в какую
     сторону или банан на канате);
  3) реализуйте операцию landBoth, поддерживающую одновременное (атомарное) приземление
     птиц на оба конца шеста, и внесите соответствующие изменения в другие функции;
  5) реализуйте операцию unlandAll (одновременный вылет всех птиц с шеста) и внесите
     соответствующие изменения в другие функции;
  4) организуйте масштабное тестирование.
-}

type Birds = Int

type Pole = (Birds, Birds)

balance = 3

updatePole :: Pole ->  Either String Pole
updatePole (ll,rr) = if unbalanced (ll,rr) then (if (ll>rr) then Left "Pole unbalanced, fell to letf" else Left "Pole unbalanced, fell to right") else Right (ll,rr)
  where
    unbalanced (l, r) = abs (l - r) > balance

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right) = updatePole (left + n, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right) = updatePole (left, right + n)


banana :: Pole -> Either String Pole
banana = const (Left "Fell because of the banana")

loadAndApply fname pole =ap ( liftM (foldr (<=<) return)  (liftM (map makeFunc) (liftM lines (readFile fname))) ) pole

makeFunc (h:s)
	| h=='R'= landRight (read s)
	| h=='L'= landLeft (read s)
	| otherwise = banana

landBoth :: Birds -> Birds -> (Birds, Birds) -> Either String Pole
landBoth nl nr (left,right) = updatePole (left+nl,right+nr)

unlandAll (left,right) = updatePole (0,0)

tests = all test [1..3]
  where
    test 1 = (return (0, 0) >>= landLeft 1 >>= landRight 4 
              >>= landLeft (-1) >>= landRight (-2)) == Left "Pole unbalanced, fell to right"
    test 2 = (return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2) == Right (2, 4)
    test 3 = (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1) == Left "Fell because of the banana"

