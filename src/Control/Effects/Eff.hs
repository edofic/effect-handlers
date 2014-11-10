module Control.Effects.Eff 
( Eff
, runEff
) where

import Control.Applicative
import Control.Effects.Class
import Control.Effects.Union
import Control.Monad.Free
import Data.Typeable

newtype Eff r a  = Eff { unEff :: Free (Union r) a } deriving (Functor, Applicative, Monad)

effEffect :: (Functor e, Member e r, Typeable e) => (forall b . (a -> Res r b) -> e (Res r b)) -> Eff r a
effEffect f = Eff $ Free $ inj $ f Prelude.return

runEff :: Eff '[] a -> a
runEff (Eff (Pure a)) = a

effHandle :: (Functor e, Typeable e) => Handler e r a b -> Eff (e ': r) a -> Eff r b
effHandle h (Eff c) = Eff $ go c where
  go (Pure a) = h $ Left a
  go (Free e) = either
    (\e -> h $ Right $ fmap go e)
    (\e -> Free $ fmap go e)
    (decomp e)

instance MonadEffect Eff where
  effect = effEffect
  runPure = runEff
  handle = effHandle 

instance Monad2 Eff where
  return = Prelude.return
  (>>=) = (Prelude.>>=)