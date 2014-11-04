import AbstractGraph
import qualified AdjacencyListGraph as AdjG
import System.Environment
import Data.List

triples [] = []
triples [_] = []
triples [_,_] = []
triples l = (take 3 l):(triples (drop 3 l))


readGraph fname = do
	content <- readFile fname ;
	let (n:f:xs) = map read $ words $ content ;
	return $ mkGraph (if (f==1) then True else False) (1,n) (map (\[a,b,c] -> (a,b,c)) (triples xs))



main = do
	[fname] <- getArgs ;
	gr1 <- readGraph fname :: IO (AdjG.Graph Int Int Int);
	putStrLn "Вершины графа:"
	print $ (nodes gr1)
	putStrLn "Дуги:"
	print $ (edges gr1)
