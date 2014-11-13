module Control.Effects.Free.State where

import Control.Effects.Free.Eff

newtype State s a = State (s -> (a,s)) deriving (Functor, Typeable)

get :: (Member (State a) r, Typeable a) => Eff r a
get = effect $ State $ \s -> (return s, s)

put :: (Member (State s) r, Typeable s) => s -> Eff r ()
put a = effect $ State $ \s -> (return (), a)

state :: (Member (State s) r, Typeable s) => (s -> (a, s)) -> Eff r a
state f = effect $ State $ \s -> 
  let (a, s') = f s
  in  (return a, s')