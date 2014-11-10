module Control.Effects.State where

import Control.Effects.Class hiding (return, (>>=))
import Control.Effects
import Data.Typeable

newtype State s a = State (s -> (a,s)) deriving (Functor, Typeable)

get :: (Member (State a) r, Typeable a, MonadEffect m) => m r a
get = effect $ \k -> State $ \s -> (k s, s)

put :: (Member (State s) r, Typeable s, MonadEffect m) => s -> m r ()
put a = effect $ \k -> State $ \s -> (k (), a)

state :: (Member (State s) r, Typeable s, MonadEffect m) => (s -> (a, s)) -> m r a
state f = effect $ \k -> State $ \s -> 
  let (a, s') = f s
  in  (k a, s')