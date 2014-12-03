import Control.Monad

{-
   Тип Parser может быть определён следуюшим образом:
-}

newtype Parser a = Parser { apply :: String -> Maybe (a, String) }

{-
   Определите экземпляры классов Monad и MonadPlus для типа Parser в этом случае:
-}

instance Monad Parser where
  return x = Parser (\s -> Just (x, s))

  p >>= q = Parser (\s -> f s (apply p s))
	where 
	f s Nothing = Nothing
	f' s pp = res
		where
		res = apply (q x) s'
		Just (x,s') = pp
	
  fail _ = Parser (\s -> Nothing)


instance MonadPlus Parser where
  mzero = Parser (\s -> Nothing)
  p `mplus` q = Parser (\s -> f (apply p s) s)
	where 
	f Nothing s = apply q s
	f pp _ = pp

