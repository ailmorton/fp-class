{- Пользуясь списком как монадой, вычислите пересечение  заданных списков -}
intersect :: Eq a => [[a]] -> [a]
intersect l = foldr (\x z -> (z >>= \y -> if (elem y x) then [y] else [])) (head l) l
