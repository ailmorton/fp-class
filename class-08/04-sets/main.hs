
import AbstractSet
import qualified ListSet as LS
import qualified TreeSet as TS
import System.Random
import Data.List

randList = do
		gen <- newStdGen
		return $ take 100 $ randomRs (1,100) gen :: IO [Int]

addList s l = foldl (\s x -> AbstractSet.insert x s) s l


testEmptySet1 s = (member 1 s1) && (member 2 s1) && (member 3 s1) && (member 4 s1) && (member 5 s1) && ( not (member 15 s1) )
		where 
			s1 = addList s [1,2,3,3,4,4,5]

testEmptySet2 s = (size (addList s [1,8,8,9,5,5,5,5,6,7,10,12])) == 8 

testSet3 s list = (length $ group $ sort list)==(size s)


main = do
		putStrLn("Результаты тестов: ")
		print (testEmptySet1 (empty :: LS.Set Int));
		print (testEmptySet1 (empty :: TS.Set Int));

		print (testEmptySet2 (empty :: LS.Set Int));
		print (testEmptySet2 (empty :: TS.Set Int));

		list <- randList;
		let s1 = addList empty list :: LS.Set Int ;
		let s2 = addList empty list :: TS.Set Int ;

		print (testSet3 s1 list) ;
		print (testSet3 s2 list) 
		
 
