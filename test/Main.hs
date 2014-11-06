module Main where

import Test.Hspec
import Test.QuickCheck
import Data.Either (isLeft)

import Examples.Misc
import qualified Examples.Reader as Rd
import qualified Examples.Exception as Exc

main :: IO ()
main = hspec $ do
  describe "A reader handler" $ do
    it "Returns the same value with simple ask" $ 
      property $ \x -> Rd.prg1res x == x

  describe "An exception handler" $ do
    it "Retruns a Left value if an exception was thrown" $
      (isLeft Exc.prg1res) `shouldBe` True

    it "Returns a Right value if no exeption was thrown" $
      property $ \x -> Exc.prg2res x == (Right $ x + 1)
      


