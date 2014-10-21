{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}

import System.Environment
import System.IO
import Data.Char
import System.Directory
import Data.List.Split 
import System.Random


main = do
  (task_num: args) <- getArgs
  do_func task_num args

do_func n args 
  | n=="1" = countLines args
  | n=="2" = addString args
  | n=="3" = toUpperFile args
  | n=="4" = mergeFiles args
  | n=="5" = randomFile args

countLines (fname:[]) = do
  withFile fname ReadMode processFile

processFile handle = do
  contents <- hGetContents handle
  putStr "Lines count = "
  print $ length $ filter (\x -> x=='\n') contents

addString (fname:str:[]) = do
  contents <- readFile fname
  writeFile "tmp.txt" (contents++str++['\n']) 
  removeFile fname
  renameFile "tmp.txt" fname

toUpperFile (fname:[]) = do
  contents <- readFile fname
  writeFile "tmp.txt" (map toUpper contents)
  removeFile fname
  renameFile "tmp.txt" fname

mergeFiles (fname1:fname2:newName:[]) = do
  contents1 <- readFile fname1
  contents2 <- readFile fname2
  writeFile newName $ concat $ map (++['\n']) $ zipWith (++) (splitOn ['\n'] contents1) (splitOn ['\n'] contents2)

randomStr = do
  gen <- getStdGen
  return $ (take 100 $ randomRs ('a','z') gen)++['\n']
  
writeRandomStr fname = do
  str <- randomStr
  appendFile fname str

randomFile (fname:[]) = do
  removeFile fname
  iterWriteStr 100 fname

iterWriteStr n fname = if n>1 then do writeRandomStr fname; iterWriteStr (n-1) fname else writeRandomStr fname







  




