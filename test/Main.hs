module Main where

import Test.Hspec
import Test.QuickCheck
import Data.Either (isLeft)

import qualified Examples.Reader as Rd
import qualified Examples.Exception as Exc
import qualified Examples.Writer as Wrt

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

  describe "Reader and Exception handler combined" $ do
    it "Handle both effects" $ do
      property $ \x ->  
        let res = Exc.prg3res x in
          if x < 15 
          then res == (Left $ show x)
          else res == (Right $ x + 1)

  describe "Default value exception handler" $ do
    it "Provides default value if exception is thrown" $ do
      property $ \d x ->  
        let res = Exc.prg4res d x in
          if x < 10
          then res == d
          else res == x + 1
  
  describe "Writer handler"$ do
    it "join the monoidal value of tell and return the result and log" $ do
      Wrt.prg1res  `shouldBe` ((), ["hello", "world"])
        
