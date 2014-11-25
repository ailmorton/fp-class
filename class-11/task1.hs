
import Control.Monad.Reader
import Control.Monad
import System.Environment

data ParamName = Summand | Multiplier | Divisor deriving (Show,Eq)

data Config = Config ParamName Integer deriving (Show)

makeConfig s = Config (paramFromString name) (read value)
	where [name,eq,value] = words s

paramFromString s
	| s=="summand" = Summand
	| s=="multiplier" = Multiplier
	| s=="divisor" = Divisor

loadConfig fname = liftM (map makeConfig) (liftM lines (readFile fname))

makePair :: FilePath ->  [Config] -> IO ([Integer], [Config])
makePair fname list = liftM2 (,) (liftM (map read) (liftM lines (readFile fname))) (return list)

applyConfig (Config param val) a 
	| Summand ==param = a+val
	| Multiplier ==param = a*val
	| otherwise = div a val


subwork :: Reader ([Integer], [Config]) [Integer]
subwork = do
	pair <- ask;
	return $ zipWith applyConfig (snd pair) (fst pair)

writeRes :: [Integer] -> IO ()
writeRes list = writeFile "result.txt" $ concat (map (\x-> (show x) ++"\n") list) 


main =  ap (liftM makePair l2) (l1 >>= loadConfig) >>= (liftM (runReader subwork))  >>= writeRes
	where 
		l1 = head `liftM` getArgs
		l2 = head `liftM` (tail `liftM` getArgs)

