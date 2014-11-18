{-
  Следующие типы задают множество состояний, алфавит и функцию переходов
  недетерминированного конечного автомата (НКА).
-}
import Data.List

type Alphabet = [Char]
type State = Int
type States = [State]
type AcceptingStates = [State]
type InitialState = State
type TransitionFunction = State -> Char -> States
type NFA = (Alphabet, States, InitialState, TransitionFunction, AcceptingStates)

-- пример НКА
nfa_ex :: NFA
nfa_ex = (['0','1'], [1, 2], 1, tf, [2])
  where
    tf 1 '0' = [1]
    tf 1 '1' = [1, 2]

-- Напишите функцию, определяющую, корректно ли задан НКА
isCorrect :: NFA -> Bool
isCorrect (alf,st,start,f,fin) = cond1 && cond2 && cond3
	where 
		cond1 = isSubset fin st
		cond2 = foldr (\x z -> (foldr (\letter zz -> (isSubset (f x letter) st) && zz) True alf) && z ) True ( (\\) st fin)
		cond3 = elem start st 		

isSubset list1 list2 = foldr (\x z -> (elem x list2) && z ) True list1


-- в дальнейшем может пригодиться функция whileM,
-- которая собирает результаты в список, пока истинно
-- заданное условие
whileM :: m Bool -> m a -> m [a]
whileM = undefined

-- Напишите функцию, определяющую, допускает ли НКА заданное слово 
accept :: NFA -> String -> Bool
accept (alf,st,start,f,fin) s = accept' (alf,st,start,f,fin) s [start]

accept' :: NFA -> String -> States -> Bool
accept' (alf,st,start,f,fin) [] q = if (intersect q fin) /= [] then True else False
accept' _ _ [] = False
accept' (alf,st,start,f,fin) s qs = accept' (alf,st,start,f,fin) (tail s) (qs >>= (\q-> f q (head s)))

-- Постройте ещё как минимум три примера НКА

-- Распознает слова вида 0..011..1
nfa1 :: NFA
nfa1 = (['0','1'], [1, 2, 3], 1, tf, [3])
  where
    tf 1 '0' = [1]
    tf 1 '1' = [2]
    tf 2 '1' = [3]
    tf 2 '0' = []
    tf 3 '1' = [3]
    tf 3 '0' = []

--Распознает слова нечетной длины
nfa2 :: NFA
nfa2 = (['0','1'], [1, 2], 1, tf, [2])
  where
    tf 1 '0' = [2]
    tf 1 '1' = [2]
    tf 2 '0' = [1]
    tf 2 '1' = [1]

--Распознает слова, в которых есть два подряд идущих нуля
nfa3 :: NFA
nfa3 = (['0','1'], [1, 2, 3], 1, tf, [3])
  where
    tf 1 '0' = [2]
    tf 1 '1' = [1]
    tf 2 '1' = [1]
    tf 2 '0' = [3]
    tf 3 '1' = [3]
    tf 3 '0' = [3]

{-
  Распределите заданные строки в соответствии с распознающими
  их НКА (одна строка может попасть в несколько групп).
-}

classify :: [NFA] -> [String] -> [(NFA, [String])]
classify = undefined
