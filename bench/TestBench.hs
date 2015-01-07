module Main where

import Criterion.Main
import qualified Examples.Free.Reader as FreeRd
import qualified Examples.Cont.Reader as ContRd
import qualified Examples.Codt.Reader as CodtRd
import qualified Examples.Free.Combined as FreeCmb
import qualified Examples.Cont.Combined as ContCmb
import qualified Examples.Codt.Combined as CodtCmb


main :: IO ()
main = do 
  defaultMain [bgroup "test1" [ bench "Free reader" $ whnf FreeRd.prg1res 1
                              , bench "Cont reader" $ whnf ContRd.prg1res 1
                              , bench "Codt reader" $ whnf CodtRd.prg1res 1],
               bgroup "test2" [ bench "Free combined" $ whnf FreeCmb.testPrgRes 100
                              , bench "Cont combined" $ whnf ContCmb.testPrgRes 100
                              , bench "Codt combined" $ whnf CodtCmb.testPrgRes 100]
              ]

