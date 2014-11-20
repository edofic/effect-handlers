module Examples.Free.Writer where

import Control.Effects.Free.Eff
import Control.Effects.Free.Writer

prg1 = do 
  tell ["hello"]
  tell ["world"]

prg1run = runEff . handle writerHandler
 
prg1res :: ((), [String])
prg1res = prg1run prg1



