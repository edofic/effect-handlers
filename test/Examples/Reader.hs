module Examples.Reader where

import Control.Effects.Eff
import Control.Effects.Reader
import Control.Effects.Exception

prg1 =  ask

prg1run x = runPure . handle (readerHandler x)

prg1res :: Int -> Int
prg1res x = prg1run x prg1
