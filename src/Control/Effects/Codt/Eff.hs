module Control.Effects.Codt.Eff
( Eff
, Handler
, Res
, effect
, runPure
, runPureRes
, handle
, inj
, Member
, Typeable
) where

import Control.Applicative
import Control.Monad.Codensity
import Control.Monad.Free
import Data.Union
import Data.Typeable

type Res r = Free (Union r)

newtype Eff r a = Eff { runEff :: Codensity (Res r) a }
                  deriving (Functor, Applicative, Monad)

type Handler e r a b = Either a (e (Res r b)) -> Res r b

effect :: (forall b . (a -> Res r b) -> Union r (Res r b)) -> Eff r a
effect e = Eff $ Codensity $ \k -> Free $ e k

runPure :: Eff '[] a -> a
runPure = runPureRes . finish

runPureRes :: Res '[] a -> a
runPureRes (Pure a) = a

finish :: Eff r a -> Res r a
finish = lowerCodensity . runEff

continue :: Res r a -> Eff r a
continue r = Eff $ Codensity (r >>=)

handleRes :: (Functor e, Typeable e) => Handler e r a b -> Res (e ': r) a -> Res r b
handleRes h (Pure a) = h $ Left a
handleRes h (Free u) = case decomp u of 
  Left  u -> h $ Right $ fmap (handleRes h) u
  Right u -> Free $ fmap (handleRes h) u

handle :: (Functor e, Typeable e) => Handler e r a b -> Eff (e ': r) a -> Eff r b
handle h c = continue $ handleRes h $ finish c
