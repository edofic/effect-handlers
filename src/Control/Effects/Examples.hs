module Control.Effects.Examples where

import Control.Effects
import Control.Effects.Reader
import Control.Effects.Exception

program1 = do
  v <- ask
  let _ = v :: Integer
  return ()

program1run n = runEff . handle (readerHandler n)

program1res :: ()
program1res = program1run (17 :: Integer) program1

-------------------

program2 = throw "something went wrong"

program2run = runEff . handle exceptionHandler

program2res :: Either String a
program2res = program2run program2

-------------------

program3 = do
  v <- ask
  if v < 15 
  then throw $ show v
  else return (v+1)

program3run n = runEff 
                 . handle exceptionHandler 
                 . handle (readerHandler n)

program3res :: Integer -> Either String Integer
program3res n = program3run n program3