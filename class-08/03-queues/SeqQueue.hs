module SeqQueue (Queue, empty, enqueue, dequeue, isEmpty) where

import AbstractQueue
import qualified Data.Sequence as Seq


newtype Queue t = Queue (Seq.Seq t)

instance AbstractQueue Queue where
  empty = Queue Seq.empty

  isEmpty (Queue a) = (Seq.length a == 0)

  enqueue (Queue a) x = Queue (x Seq.<| a)

  dequeue (Queue a) = (y, (Queue ys))
		where (ys Seq.:> y) = Seq.viewr a

