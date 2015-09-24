-- This module provides the familiar state effect.
module Control.Effects.State where

import Control.Effects.Eff
import Control.Monad


-- |The functor representing the effect. You shouldn't need
-- to create this manually, just use `get`, `put` or `state`.
data State s a = SGet (s -> a)
               | SPut s a
               deriving (Functor)

-- |Read from state
get :: (Member (State a) r) => Eff r a
get = effect $ \k -> inj $ SGet k

-- |Write to state
put :: (Member (State s) r) => s -> Eff r ()
put a = effect $ \k -> inj $ SPut a $ k ()

-- |Lift a function into state
state :: (Member (State s) r) => (s -> (a, s)) -> Eff r a
state f = do
  s <- get
  let (a, s') = f s
  put s'
  return a

-- |Handle state into a function. Note that applying the resulting
-- function you get out another program that you have to bind over.
stateHandler :: Handler (State s) r a (s -> Eff r a)
stateHandler (Value a) = return $ const $ return a
stateHandler (Comp (SGet k)) = return $ \s -> join $ ($s) `fmap` continue (k s)
stateHandler (Comp (SPut s k)) = return $ \_ -> join $ continue $ fmap ($s) k
