import Control.Monad

testPrg = do
  v <- ask
  x <- choose [1..(v :: Int)]
  when (x < 5) $ searchFail (T :: T Int)
  y <- choose [3,4]
  let d = x - y
  if d == 0
  then throw $ "division by zero"
  else return ((fromIntegral v) / (fromIntegral d))


testPrgRun n = runPure 
                 . handle exceptionHandler 
                 . handle (handleDFS :: Handler (Search Int) r a [a])
                 . handle (readerHandler n)

testPrgRun2 n = runPure 
                 . handle (handleDFS :: Handler (Search Int) r a [a])
                 . handle exceptionHandler 
                 . handle (readerHandler n)

 
testPrgRes :: Int -> Either String [Float]
testPrgRes n = testPrgRun n testPrg

testPrgRes2 :: Int -> [Either String Float]
testPrgRes2 n = testPrgRun2 n testPrg


