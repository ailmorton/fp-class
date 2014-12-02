{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char
import System.Environment

isValid :: String -> (Int,Bool,Bool,Bool) -> Bool
isValid s (n,c1,c2,c3) = length s >= n && 
                (any isAlpha s || not c1) && 
                (any isNumber s || not c2) && 
                (any isPunctuation s || not c3)


getValidPassword :: MaybeT (WriterT [String] (ReaderT (Int,Bool,Bool,Bool) IO)) String
getValidPassword = do
  lift $ lift $ lift $ putStrLn "Введите новый пароль:"
  s <- lift $ lift $ lift $ getLine ;
	tell [s] ;
	cond <- ask
  guard (isValid s cond)
  return s
 

askPassword :: MaybeT (WriterT [String] (ReaderT (Int,Bool,Bool,Bool) IO)) ()
askPassword = do
  value <- msum $ repeat getValidPassword
  lift $ lift $ lift $ putStrLn "Сохранение в базе данных..."



main = liftM f getArgs >>= runReaderT (runWriterT (runMaybeT askPassword))

f ::  [String] -> (Int, Bool, Bool, Bool)
f [n,c1,c2,c3] = (read n, if (c1=="true") then True else False, if (c2=="true") then True else False,if (c3=="true") then True else False)


