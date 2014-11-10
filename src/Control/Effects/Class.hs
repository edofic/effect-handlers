{-# LANGUAGE PolyKinds #-}

module Control.Effects.Class where

import Control.Monad.Free
import Control.Effects.Union
import Data.Typeable

type Res r = Free (Union r)
type Handler e r a b = Either a (e (Res r b)) -> Res r b

runPureRes :: Res '[] a -> a
runPureRes (Pure a) = a

class Monad2 (m :: k -> * -> *) where 
  return :: a -> m r a
  (>>=) :: m r a -> (a -> m r b) -> m r b

class (Monad2 m) => MonadEffect m where
  effect :: (Functor e, Member e r, Typeable e) => (forall b . (a -> Res r b) -> e (Res r b)) -> m r a
  handle :: (Functor e, Typeable e) => Handler e r a b -> m (e ': r) a -> m r b
  runPure :: m '[]  a -> a
