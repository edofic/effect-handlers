-- This module provides the familiar writer effect.
module Control.Effects.Writer where

import Control.Effects.Eff
import Data.Monoid


-- |The functor representing the effect. You shouldn't need
-- to create this manually, just use `tell`
data Writer m a = Writer m a deriving (Functor)

-- |Send a value into the writer
tell :: (Member (Writer m) r) => m -> Eff r ()
tell m = effect $ \k -> inj $ Writer m $ k ()

-- |Handles writes by `mappend`ing them together.
writerHandler :: (Monoid m) => Handler (Writer m) r a (a, m)
writerHandler (Value a) = return (a, mempty)
writerHandler (Comp (Writer m k)) = do
  (a, m') <- k
  return (a, m <> m')
