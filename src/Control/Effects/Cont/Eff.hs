-- |This implementation of the efffect monad uses hand rolled Free
-- with hand rolled "inlined" codensity transformation (CPS-ish).
-- It is supposed to be the fastest since it has no extra abstractions.
module Control.Effects.Cont.Eff 
( Eff
, Handler
, Res
, effect
, runPure
, runPureRes
, handle
, finish
, continue
, inj
, Member
, Typeable
) where

import Control.Applicative
import Control.Monad.Cont 
import Control.Monad.Free
import Data.Union
import Data.Typeable

-- |Result structure of the program is isomorphic to 
-- `Free (Union r) a`
data Res r a = Val a | E (Union r (Res r a))
             deriving Functor 

instance Applicative (Res r) where
  pure = Val
  mf <*> ma = mf >>= flip fmap ma

instance Monad (Res r) where
  return = Val
  (Val a) >>= f = f a
  (E u) >>= f = E $ fmap (>>= f) u

newtype Eff r a = Eff { runEff :: forall b . (a -> Res r b) -> Res r b}

instance Functor (Eff r) where
  fmap f eff = Eff $ \k -> runEff eff (k . f)

instance Applicative (Eff r) where
  pure a = Eff ($a)
  mf <*> mx = Eff $ \k -> 
              runEff mf $ \f ->
              runEff mx $ k . f

instance Monad (Eff r) where
  return a = Eff ($a)
  m >>= f = Eff $ \k -> 
            runEff m $ \a ->
            runEff (f a ) k

-- |Handler is a function that takes a result or an effect and a continuation
-- |and handles it. 
-- 
-- `e` is the effect functor you are handling
--
-- `r` represents the type of the type list of the remaining effects. 
--  Usually you want to be polymorphic in this.
--
-- `a` is the result type of the program you will handle
--
-- `b` is the result of handled computation.
type Handler e r a b = Either a (e (Res r b)) -> Res r b

-- | `effect` is meant to be used as a helper function for defining new effects.
-- See predefined effects for examples. Good way to use it is to pass in a lambda
-- expression with explicit `k` for continuation. You will need to manually `inj` 
-- into the `Union` because of some GHC limitations.
effect :: (forall b . (a -> Res r b) -> Union r (Res r b)) -> Eff r a
effect e = Eff $ \k -> E $ e k

-- |A program without effects is guaranteed to be pure so you 
-- can safely convert it into a value.
runPure :: Eff '[] a -> a
runPure = runPureRes . finish

-- |Like `runPure` but for program results. You only need this for implementing
-- some handlers. 
runPureRes :: Res '[] a -> a
runPureRes (Val a) = a

-- |Finish a program and convert it into a result structure.
finish :: Eff r a -> Res r a
finish c = runEff c Val

-- |Convert a result back into a program in order to compose it.
-- This function might not be needed and might introduce some 
-- performance issues (it is used in `handle`) but we didn't find 
-- a way to drop it.
continue :: Res r a -> Eff r a
continue r = Eff (r >>=)


handleRes :: (Functor e, Typeable e) => Handler e r a b -> Res (e ': r) a -> Res r b
handleRes h (Val a) = h $ Left a
handleRes h (E u) = case decomp u of 
  Left  u -> h $ Right $ fmap (handleRes h) u
  Right u -> E $ fmap (handleRes h) u

-- |Use a `Handler` on an `Eff` program to stripe away the first layer of effects.
-- There are some issues if you are using a handler that is somewhat polymorphic in `e`
-- As the compiler cannot figure out which effect are you handling. Currently the best 
-- solution seems to be to manually specify type of the handler such that it is monomorphic
-- in `e`. Sorry.
handle :: (Functor e, Typeable e) => Handler e r a b -> Eff (e ': r) a -> Eff r b
handle h c = continue $ handleRes h $ finish c
