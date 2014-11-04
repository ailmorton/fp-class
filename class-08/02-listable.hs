{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}
class Listable a where
	toList :: a -> [a]
	fromList :: [a] -> a

{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integer - любое целое число разбивается на список цифр.
-}

instance Listable String where
	toList a = words a
	fromList a = concat a

digits a 
	| a<0 = digits (abs a)
	| a<10 = [a]
	| otherwise =(digits (div a 10))++[(mod a 10)] 


concat_number digits = c_number (reverse digits) 0
	where
		c_number [x] n = x * 10^n
		c_number (x:xs) n = x * 10^n + (c_number xs (n+1))

instance Listable Integer where
	toList a = digits a
	fromList a = concat_number a
