{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}
import System.Environment
import Control.Monad
import Data.List

data Student = Student String Int Int deriving (Show)

triples [] = []
triples [_] = []
triples [_,_] = []
triples l = (take 3 l):triples (drop 3 l)  

loadData fname = liftM (map (\[name,age,gr] -> Student name (read age) (read gr))) (liftM triples (liftM words (readFile fname)))

uniteStudents list1 list2 = sortBy (\(Student n1 b c) (Student n2 d e) -> compare n1 n2) (list1++list2)

toStrings l = concat $ foldr (\(Student name age gr) z -> name:" ":(show age):" ":(show gr):"\n":z) [] l

writeData fname l = writeFile fname (toStrings l)

myAction [fname1,fname2] = uniteStudents `liftM` (loadData fname1) `ap` (loadData fname2) 

main = getArgs >>= myAction >>= (writeData  "result.txt")



