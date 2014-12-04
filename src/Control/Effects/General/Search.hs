import Control.Monad

data T a = T

asT :: a -> T a -> a
asT a T = a

data Search w a = SChoose [w] (w -> a) deriving (Functor, Typeable)

choose :: (Member (Search w) r, Typeable w) => [w] -> Eff r w
choose ws = effect $ \k -> inj $ SChoose ws k

searchFail :: (Member (Search w) r, Typeable w) => T w -> Eff r ()
searchFail t = do
  x <- choose []
  let _ = x `asT` t
  return ()

handleDFS :: Handler (Search w) r a [a] 
handleDFS (Left a) = return [a]
handleDFS (Right (SChoose ws k)) = f $ map k ws where
  f = foldr (liftM2 (++)) $ return []