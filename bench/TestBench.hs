module Main where

import Criterion.Main
import qualified Examples.Free.Reader as FreeRd
import qualified Examples.Cont.Reader as ContRd
import qualified Examples.Codt.Reader as CodtRd

main :: IO ()
main = do 
  defaultMain [bgroup "test" [ bench "Free reader" $ whnf FreeRd.prg1res 1
                             , bench "Cont reader" $ whnf ContRd.prg1res 1
                             , bench "Codt reader" $ whnf CodtRd.prg1res 1]
              ]

