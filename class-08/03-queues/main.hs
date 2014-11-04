
import AbstractQueue
import qualified Queue as Q
import qualified FastQueue as FQ
import qualified SeqQueue as SQ
import System.Random
import Control.Monad
import System.Environment

randList = do
		gen <- newStdGen
		return $ randomRs (1,10) gen :: IO [Int]

addList q l = foldl (\q x -> enqueue q x) q l

remN q n = foldr (\x q -> snd (dequeue q) ) q [1..n]

addRemN n list q = remN (addList q (take n list)) (n-1)

addRemQueue q list n
	| n>0 = addRemQueue (addRemN n list q) list (n-1)
	| otherwise = q

checkQueues q1 q2 q3 n
	| (isEmpty q1) && (isEmpty q2) && (isEmpty q3) = (True,n)
  | (isEmpty q1) || (isEmpty q2)|| (isEmpty q3) = (False,n)
	| otherwise = if ((fst q1') /= (fst q2') || (fst q1') /= (fst q3') || (fst q2') /= (fst q3')) then (False,n) else (checkQueues (snd q1') (snd q2') (snd q3') (n+1) )
		where
			q1' = dequeue q1
			q2' = dequeue q2
			q3' = dequeue q3
			
printQueue q
	| isEmpty q = return  ()	
	| otherwise = do
			print $ fst (dequeue q) ;
			printQueue $ snd (dequeue q)

main = do
		[n] <- getArgs
		list <- randList;
		let q1 = addRemQueue empty list (read n::Int) :: Q.Queue Int
		let q2 = addRemQueue empty list (read n::Int) :: FQ.Queue Int
		let q3 = addRemQueue empty list (read n::Int) :: SQ.Queue Int
		let res = checkQueues q1 q2 q3 0
		if (fst res) then putStrLn ("Элементы совпадают, количество = "++(show (snd res))) else putStrLn "Элементы не совпадают"
 
