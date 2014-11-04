module AdjacencyListGraph (Graph, mkGraph, edgeIn, adjacent, nodes, weight, edges, fromGraph, nodesBounds ) where

import AbstractGraph
import Data.Array.IArray


makeAdjListK False (b1,b2) list k = [ (n2,w) | (n1,n2,w) <- list,n1==k ]
makeAdjListK True (b1,b2) list k = [ (n2,w) | (n1,n2,w) <- list,n1==k ]++[ (n1,w) | (n1,n2,w) <- list,n2==k ]

makeAdjList f (n1,n2) list =  [makeAdjListK f (n1,n2) list i | i <- range (n1,n2)]

edgeInList (n,w) list i = foldr (\(n1,n2,w) z -> if n1==i && n2==n then True else z) False list

addEdges list edges n = foldr (\(n1,w) z -> if (edgeInList (n1,w) z n)then z else (n,n1,w):z ) list edges

newtype Graph g n w = Graph (Array n [(n,w)])

instance AbstractGraph (Graph g) where

	mkGraph f (n1,n2) list = Graph (listArray (n1,n2) (makeAdjList f (n1,n2) list ))

	edgeIn (Graph (arr)) (n1,n2) = foldr (\(n,x) f -> if (n==n2) then True else f ) False (arr ! n1)

	adjacent (Graph (arr)) n = map (\(n1,x)->n1) (arr ! n)

	nodes (Graph (arr)) = range $ bounds arr

	weight n1 n2 (Graph (arr)) = foldr (\(n,w) z -> if n==n2 then w else z) (-1) (arr ! n1)

	edges (Graph (arr)) = foldr (\n z -> addEdges z (arr ! n) n) [] (range (bounds arr))

	fromGraph g = mkGraph False (nodesBounds g) (edges g)

	nodesBounds (Graph (arr)) = bounds arr

