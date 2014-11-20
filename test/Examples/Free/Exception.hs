module Examples.Free.Exception where

import Control.Effects.Free.Eff
import Control.Effects.Free.Exception
import Control.Effects.Free.Reader

handleException = runEff . handle exceptionHandler

-------------------------------------------------------

prg1 = throw "something went wrong"

prg1res :: Either String a
prg1res = handleException prg1

-------------------------------------------------------

prg2 n = return (n + 1)

prg2res :: Integer -> Either String Integer
prg2res n = handleException  $ prg2 n

-------------------------------------------------------

prg3 = do
  v <- ask
  if v < 15 
  then throw $ show v
  else return (v+1)

prg3run n = runEff 
                 . handle exceptionHandler 
                 . handle (readerHandler n)

prg3res :: Integer -> Either String Integer
prg3res n = prg3run n prg3

-------------------------------------------------------

prg4 = do
  v <- ask
  if v < 10
  then throw "Not a valid number"
  else return (v + 1)

prg4run :: Integer -> Integer -> Eff '[Reader Integer, Exception String] Integer -> Integer
prg4run d n = runEff
                  . handle (defValueExeptionHandler d) 
                  . handle (readerHandler n)

prg4res :: Integer -> Integer -> Integer
prg4res d n = prg4run d n prg4

-------------------------------------------------------
