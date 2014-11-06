module Examples.Exception where

import Control.Effects
import Control.Effects.Exception
import Control.Effects.Reader

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
