{-
   Представленный на лекции вычислитель выражений в обратной польской нотации
   не проверяет корректность заданного выражения, выдавая, к примеру, следующие
   очевидно неправильные ответы:

   ghci> evalRPN "* 1"
   1
   ghci> evalRPN "+ * 2 4"
   4
   ghci> evalRPN "* * *"
   *** Exception: 01-polish-calc.hs:10:15-43: Non-exhaustive patterns in lambda

   1. Переработайте реализацию вычислителя таким образом, чтобы в случае ошибки ответ
   не выводился. Воспользуйтесь в решении монадой Maybe, совместив её с монадой State
   с помощью преобразователя монад.

   2. Добавьте к вычислителю поддержку переменных. Значения переменных должны
   задаваться в командной строке, а доступ к ним должен осуществляться средствами
   монады Reader.

   3. Добавьте к вычислителю подсчёт количества операций со стеком (монада Writer
   с журналом типа Sum Int).
-}


import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

type Stack = [Int]

push :: Int -> MaybeT (State Stack) ()
push x = get >>= put . (x:)

pop :: MaybeT (State Stack) Int
pop = get >>= \s -> if (s==[]) then MaybeT (return Nothing) else do put (tail s);  MaybeT (return (Just (head s))) 


evalRPN xs = if (f==[]) then return () else print (head f)
  where
		f = execState (runMaybeT (mapM step $ words xs) ) []
		step "+" = processTops (+)
		step "*" = processTops (*)
		step  n  = push (read n)
		processTops op = op `liftM`  pop `ap` pop >>= push



