module Main where

import Test.Hspec
import Test.QuickCheck

import Examples.Reader
import Examples.Misc

main :: IO ()
main = hspec $ do
  describe "A reader handler" $ do
    it "Returns the same value with simple ask" $ 
      property $ \x -> prgReader1res x == x

