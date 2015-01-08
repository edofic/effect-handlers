module Main where

import Criterion.Main
import qualified Examples.Reader as Rd
import qualified Examples.Combined as Cmb


main :: IO ()
main = do 
  defaultMain [bgroup "test1" [ bench "reader" $ whnf Rd.prg1res 1],
               bgroup "test2" [ bench "combined" $ whnf Cmb.testPrgRes 100]
              ]

