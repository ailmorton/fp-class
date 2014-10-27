{-
  Соберите следующую информацию по текстовому файлу kapitan.txt:

  1) самый часто используемый знак препинания;
  2) 50 наиболее часто используемых в тексте слов (с указанием количества использований);
  3) частоты символов, биграмм и триграмм (вывести соответствующую информацию для
     наиболее часто встречающихся);
  4) количества использованных предлогов, местоимений и имён собственных.

  Постарайтесь использовать в решении наиболее подходящие структуры данных, избегайте повторные
  вычисления и заведомо неэффективные операции. Проверьте написанную программу на трёх других
  текстах того же или большего размера. Сравните результаты.
-}


import System.Environment
import System.Directory
import System.IO
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Set as Set


cmp a b 
  | isLetter a /= isLetter b = False
  | isPunctuation a /= isPunctuation b = False
  | otherwise = True

sep_punct (c:chrs) (l1,l2) 
  | isLetter c = ((c:chrs):l1,l2) 
  | isPunctuation c = (l1,(c:chrs):l2) 
  | otherwise = (l1,l2) 

analyze content = foldr sep_punct ([],[]) ( groupBy cmp content)


pair_cmp (a1,a) (b1,b)
	| a==b = EQ
  | a<b = GT
  | otherwise = LT 
  
punct_analyze l = do 
  let res = sortBy pair_cmp $ map (\(x:xs) -> (x, length (x:xs)) ) $ group $ sort $ snd l
  putStr "Самый часто используемый знак препинания: "
  putStrLn $ fst $ head res 

wrds_analyze l = do 
  let res = sortBy pair_cmp $ map (\(x:xs) -> (x, length (x:xs)) ) $ group $ sort $ fst l
  putStrLn "Самые часто используемые слова: "
  print_words ( take 50 res )
  putStrLn "" ;
	return res

print_words l 
  | length l ==1 = do
		putStr $ (fst (head l))++"("++(show (snd (head l)))++") "
  | otherwise = do
		putStr $ (fst (head l))++"("++(show (snd (head l)))++") "
		print_words $ tail l

sep_words content = do
  return $ analyze content

letters_analyze l = do
  let res =  head $ sortBy pair_cmp $ map (\(x:xs) -> (x, length (x:xs)) ) $ group $ sort $ concat $ fst l
  putStr "Самая часто используемая буква: "
  putStrLn $ [fst res]++"("++(show (snd res))++")"

bigramm [] = []
bigramm [_] = []
bigramm l = (take 2 l):(bigramm (tail l))

trigramm [] = []
trigramm [_] = []
trigramm [_,_] = []
trigramm l = (take 3 l):(trigramm (tail l))

bigramm_analyze l = do
  let res =  head $ sortBy pair_cmp $ map (\(x:xs) -> (x, length (x:xs)) ) $ group $ sort $ concat $ map bigramm $ fst l
  putStr "Самая часто используемая биграмма: "
  putStrLn $ fst res++"("++(show (snd res))++")"

trigramm_analyze l = do
  let res =  head $ sortBy pair_cmp $ map (\(x:xs) -> (x, length (x:xs)) ) $ group $ sort $ concat $ map trigramm $ fst l
  putStr "Самая часто используемая триграмма: "
  putStrLn $ fst res++"("++(show (snd res))++")"

find_not_let [] = 0
find_not_let (x:xs) 
	| isLetter x = find_not_let(xs)+1
	| otherwise = 0

find_prop_names [] = Set.empty
find_prop_names [_] = Set.empty
find_prop_names (x1:x2:xs) 
	| isUpper x2 && x1 /= '.' = Set.insert (take (find_not_let (x2:xs)) (x2:xs)) (find_prop_names (x2:xs) )
  | otherwise = find_prop_names (x2:xs) 

speech_parts l content = do
  putStr "Количество используемых предлогов: " ;
	putStrLn $ show $ Set.size res1 ;
	putStr "Количество используемых местоимений: " ;
	putStrLn $ show $ Set.size res2 ;
	putStr "Количество используемых имен собственных: " ;
	putStrLn $ show $ Set.size (find_prop_names content);
  where
		pred = Set.fromList ["в", "без", "до", "из", "к", "на", "по", "о", "от", "перед", "при", "через", "с", "у", "за", "над", "об", "под", "про", "для", "вблизи", "вглубь", "вдоль", "возле", "около", "вокруг", "впереди", "после", "посредством", "путём", "насчёт", "ввиду"] ;
		pronoun = Set.fromList ["я", "что", "он", "этот", "они", "мы", "весь", "который", "она", "свой", "вы", "ты", "такой", "его", "себя", "ее", "другой", "наш", "самый", "мой", "кто", "сам", "там", "какой", "их", "потом", "ничто", "каждый", "потому", "тогда", "здесь", "всегда", "ваш", "никто", "почему", "поэтому", "свое", "никогда", "никакой", "некоторый", "твой", "куда", "зачем", "сей", "туда", "всего", "откуда", "сюда", "столь", "некий", "чего", "отсюда", "чей", "вон", "оттуда", "таков", "никуда", "каков", "таковой", "кой", "оттого", "некогда", "отчего", "нигде", "всюду", "ничуть", "сколь", "эдакий", "некто", "отовсюду", "ничей", "доселе", "оный", "ниоткуда", "экий"] ;
		(res1,res2) = foldr (\x (l1,l2) -> if Set.member (map toLower (fst x)) pred then (Set.insert x l1,l2) else (if Set.member (map toLower(fst x)) pronoun then (l1,Set.insert x l2) else (l1,l2)  ) ) (Set.empty,Set.empty) l 
	

main = do
  [fname] <- getArgs 
  content <- readFile fname 
  wrds_puncts <- sep_words content ;
  punct_analyze wrds_puncts
  stat_wrds <- wrds_analyze wrds_puncts
  letters_analyze wrds_puncts
  bigramm_analyze wrds_puncts
  trigramm_analyze wrds_puncts
  speech_parts stat_wrds content

