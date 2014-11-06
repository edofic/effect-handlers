module Examples.Reader where

import Control.Effects
import Control.Effects.Reader
import Control.Effects.Exception


prgReader1 =  ask

prgReader1run x = runEff . handle (readerHandler x)

prgReader1res :: Int -> Int
prgReader1res x = prgReader1run x prgReader1

