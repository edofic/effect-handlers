module Examples.Reader where


import Control.Effects
import Control.Effects.Reader
import Control.Effects.Exception


prg1 =  ask

prg1run x = handle (readerHandler x)

prg1res :: (MonadEffect m) => Int -> m '[] Int
prg1res x = prg1run x prg1
