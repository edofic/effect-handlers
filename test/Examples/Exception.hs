{-# LANGUAGE RebindableSyntax #-}

module Examples.Exception where

import Prelude hiding (return, (>>=))
import Control.Effects
import Control.Effects.Exception
import Control.Effects.Reader

handleException = handle exceptionHandler

-------------------------------------------------------

prg1 = throw "something went wrong"

prg1han = handleException 

-------------------------------------------------------

prg2 n = return (n + 1)

prg2res n = handleException  $ prg2 n

-------------------------------------------------------

prg3 = do
  v <- ask
  case v < 15 of
    True -> throw $ show v
    False -> return $ v + 1

prg3run n =   handle exceptionHandler 
            . handle (readerHandler n)

prg3res n = prg3run n prg3

-------------------------------------------------------

prg4 = do
  v <- ask
  case v < 10 of
    True -> throw "Not a valid number"
    False -> return (v + 1)

prg4run d n =  handle (defValueExeptionHandler d) 
             . handle (readerHandler n)

prg4res d n = prg4run d n prg4

-------------------------------------------------------
