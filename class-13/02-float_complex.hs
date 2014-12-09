import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.Char

{- Напишите парсер для вещественных чисел. -}

float :: Parser Float
float =  Parser ff
	where
	l1 s = apply (many digit >> symbol "." >> many digit) s -- !
	f1 [] = 0.0
	f1 l = fst (foldl (\(sum,p) x -> (sum + (fromIntegral x)/p,p*10)) (0,10) l)
	f2 s1 = parse (many digit) s1
	f [] _  _ _= []
	f list [] min s1= [((fst (foldr (\x (sum,p)-> (sum+(fromIntegral x)*p,p*10)) (0,1) list))*min, snd(head (apply (many digit) s1)) )]
	f list s2 min _= [((f1 (fst(head s2)) )+ (fst (foldr (\x (sum,p)-> (sum+(fromIntegral x)*p,p*10)) (0,1) list))*min, snd(head s2) )]
	ff ('-':s) = f (f2 s) (l1 s) (-1) s
	ff s = f (f2 s) (l1 s) 1 s


{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
  
-}
complex :: Parser (Float, Float)
complex = do
		symbol "("
		n1 <- float
		symbol ","
		n2 <- float
		symbol ")"
		return (n1,n2)

{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}

complexList :: Parser [(Float, Float)]
complexList = do
	char '['
	list <- sepBy complex (char ';') ;
	char ']'
	return list
  

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complexList2 :: Parser [(Float, Float)]
complexList2 = do
	char '['
	list <- sepBy (floatToComp <|> complex) (char ';') ;
	char ']'
	return list

floatToComp :: Parser (Float, Float)
floatToComp = do
		n1 <- float
		return (n1,0)


{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = undefined


