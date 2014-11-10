module Control.Effects.EffC2 where

import Control.Effects.Class  
import Control.Effects.Union
import Control.Applicative
import Control.Monad.Free
import Data.Typeable

newtype EffC2 r a = EffC2 { unEffC2 :: forall b . (a -> Res r b) -> Res r b}

instance Functor (EffC2 r) where
  fmap f c = EffC2 $ \k -> unEffC2 c (k . f)

instance Applicative (EffC2 r) where
  pure a = EffC2 ($a)
  mf <*> mx = EffC2 $ \k -> 
              unEffC2 mf $ \f ->
              unEffC2 mx $ k . f

instance Monad (EffC2 r) where
  return a = EffC2 ($a)
  m >>= f = EffC2 $ \k -> 
            unEffC2 m $ \a ->
            unEffC2 (f a ) k

effc2Effect :: (Functor e, Member e r, Typeable e) => 
  (forall b . (a -> Res r b) -> e (Res r b)) -> EffC2 r a
effc2Effect e = EffC2 $ \k -> Free $ inj $ e k

runEffC2 :: EffC2 '[] a -> a
runEffC2 e = a where
  Pure a = unEffC2 e Pure

finish :: EffC2 r a -> Res r a
finish c = unEffC2 c Pure

continue :: Res r a -> EffC2 r a
continue r = EffC2 (r Prelude.>>=)


handleRes :: (Functor e, Typeable e) => Handler e r a b -> Res (e ': r) a -> Res r b
handleRes h (Pure a) = h $ Left a
handleRes h (Free u) = case decomp u of 
  Left  u -> h $ Right $ fmap (handleRes h) u
  Right u -> Free $ fmap (handleRes h) u

effc2Handle :: (Functor e, Typeable e) => Handler e r a b -> EffC2 (e ': r) a -> EffC2 r b
effc2Handle h c = continue $ handleRes h $ finish c

instance MonadEffect EffC2 where
  effect = effc2Effect
  runPure = runEffC2
  handle = effc2Handle

instance Monad2 EffC2 where
  return = Prelude.return
  (>>=) = (Prelude.>>=)