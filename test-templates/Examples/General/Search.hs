import Control.Monad

prg1 :: Member (Search Int) r => Eff r (Int, Int)
prg1 = do
  x <- choose [1,2]
  when (x > 1) $ searchFail (T :: T Int)
  y <- choose [3,4]
  return (x,y)

prg1res = runPure $ handle (handleDFS :: Handler (Search Int) r a [a]) $ prg1 