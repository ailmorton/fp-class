import Control.Monad.State

type Queue = [Int]

enqueue :: Int -> State Queue ()
enqueue x = do
	xs <- get
	put (xs++[x])

dequeue :: State Queue Int
dequeue = do
	(x:xs) <- get
	put xs
	return x

queueManip :: State Queue Int
queueManip = do
	enqueue 3
	a <- dequeue
	dequeue


{-
> runState queueManip [5,8,2,1]
(8,[2,1,3])

> runState queueManip [9]
(3,[])

-}
