module Examples.Writer where

import Control.Effects.Eff
import Control.Effects.Writer

prg1 = do 
  tell ["hello"]
  tell ["world"]

prg1run = runPure . handle writerHandler
 
prg1res :: ((), [String])
prg1res = prg1run prg1
