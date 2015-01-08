import Control.Monad

-- $desc
-- This module provides the familiar reader effect.

-- |A proxy for passing type to functions. Example
-- > foo
data T a = T

asT :: a -> T a -> a
asT a T = a

-- |The functor representing the effect. You shouldn't need
-- to create this manually, just use `choose` or `searchFail`.
data Search w a = SChoose [w] (w -> a) deriving (Functor, Typeable)

-- |Nondeterministicaly choose an element from a list
choose :: (Member (Search w) r, Typeable w) => [w] -> Eff r w
choose ws = effect $ \k -> inj $ SChoose ws k

-- |Fail a search. Equal to choosing from an empty list.
searchFail :: (Member (Search w) r, Typeable w) => T w -> Eff r ()
searchFail t = do
  x <- choose []
  let _ = x `asT` t
  return ()

-- |Use a strict depth first search. Equal to using `ListT`
handleDFS :: Handler (Search w) r a [a] 
handleDFS (Value a) = return [a]
handleDFS (Comp (SChoose ws k)) = f $ map k ws where
  f = foldr (liftM2 (++)) $ return []

-- |Lazy depth first search with backtracking.
handleBacktrackMaybe :: Handler (Search w) r a (Maybe a)
handleBacktrackMaybe (Value a) = return $ Just a
handleBacktrackMaybe (Comp (SChoose ws k)) = step ws where
  step [] = return Nothing
  step (w:ws') = 
    k w >>= \r -> case r of 
      m@(Just x) -> return m
      Nothing -> step ws'
