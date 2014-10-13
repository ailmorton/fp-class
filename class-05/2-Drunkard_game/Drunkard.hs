{-# LANGUAGE EmptyDataDecls #-}

module Drunkard  where

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 52 карты.
-}

data Suit = Spades
          | Clubs
          | Diamonds
          | Hearts
          deriving(Eq,Show)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
            deriving(Eq,Show,Ord)

data Card = Card Value Suit
            deriving(Show)


-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit (Card _ s1) (Card _ s2) = s1 == s2


{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}

beats :: Card -> Card -> Ordering
(Card v1 _) `beats` (Card v2 _) | v1 > v2 = GT
                                | v1 < v2 = LT
                                | otherwise = EQ

{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры): 
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

takeSame :: ([Card], [Card]) -> ([Card], ([Card], [Card]))
takeSame ((x:xs),(y:ys))
  | x `beats` y/=EQ = ([],(x:xs,y:ys))
  | otherwise = (y:x: (fst list), snd list)
    where list = takeSame (xs,ys)
takeSame lists = ([],lists)

game_round :: ([Card], [Card]) -> ([Card], [Card])
game_round cards1
  | x `beats` y==GT = (xs++[x,y]++sameList,ys)
  | y `beats` x==GT = (xs,ys++[x,y]++sameList)
  where cards = takeSame cards1  
        (x:xs) = fst $ snd cards
        (y:ys) = snd $ snd cards
        sameList = fst cards
        

{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second deriving (Show,Eq)

game :: ([Card], [Card]) -> (Winner, Int)
game ([],_) = (Second,0) 
game (_,[]) = (First,0) 
game cards = (fst res, 1+snd res)
  where res = game $ game_round cards

{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}

l1 = [Card King Hearts, Card Ten Diamonds, Card Jack Spades, Card Queen Clubs, Card Six Diamonds]
l2 = [Card Queen Hearts, Card Jack Spades, Card Two Clubs,Card Three Hearts, Card Four Spades]


game_test1 = game (l1, l2) == (First,7)

{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
-}
