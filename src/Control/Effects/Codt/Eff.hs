-- |This implementation of the effect monad uses `Free` over a 
-- `Union` of functors and then applies `Codensity` over it
-- for asymptotic improvements of ill-associated binds.
module Control.Effects.Codt.Eff
( Eff
, Handler
, Comp (Value, Comp)
, Res
, effect
, runPure
, runPureRes
, handle
, continue
, finish
, inj
, Member
, Typeable
) where

import Control.Applicative
import Control.Monad.Codensity
import Control.Monad.Free
import Data.Union
import Data.Typeable

-- |Result structure of the program is directly `Free` over `Union` 
-- indexed by the list of effect functors.
type Res r = Free (Union r)

newtype Eff r a = Eff { runEff :: Codensity (Res r) a }
                  deriving (Functor, Applicative, Monad)

-- |Comp represents a computation. It is either a pure value or a computation 
-- that needs further evaluation and effect handling. 
data Comp e r a b = Value a | Comp (e (Res r b))

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
type Handler e r a b = Comp e r a b -> Res r b

-- | `effect` is meant to be used as a helper function for defining new effects.
-- See predefined effects for examples. Good way to use it is to pass in a lambda
-- expression with explicit `k` for continuation. You will need to manually `inj` 
-- into the `Union` because of some GHC limitations.
effect :: (forall b . (a -> Res r b) -> Union r (Res r b)) -> Eff r a
effect e = Eff $ Codensity $ \k -> Free $ e k

-- |A program without effects is guaranteed to be pure so you 
-- can safely convert it into a value.
runPure :: Eff '[] a -> a
runPure = runPureRes . finish

-- |Like `runPure` but for program results. You only need this for implementing
-- some handlers. 
runPureRes :: Res '[] a -> a
runPureRes (Pure a) = a

-- |Finish a program and convert it into a result structure.
finish :: Eff r a -> Res r a
finish = lowerCodensity . runEff

-- |Convert a result back into a program in order to compose it.
-- This function might not be needed and might introduce some 
-- performance issues (it is used in `handle`) but we didn't find 
-- a way to drop it.
continue :: Res r a -> Eff r a
continue r = Eff $ Codensity (r >>=)

handleRes :: (Functor e, Typeable e) => Handler e r a b -> Res (e ': r) a -> Res r b
handleRes h (Pure a) = h $ Value a
handleRes h (Free u) = case decomp u of 
  Left  u -> h $ Comp $ fmap (handleRes h) u
  Right u -> Free $ fmap (handleRes h) u

-- |Use a `Handler` on an `Eff` program to stripe away the first layer of effects.
-- There are some issues if you are using a handler that is somewhat polymorphic in `e`
-- As the compiler cannot figure out which effect are you handling. Currently the best 
-- solution seems to be to manually specify type of the handler such that it is monomorphic
-- in `e`. Sorry.
handle :: (Functor e, Typeable e) => Handler e r a b -> Eff (e ': r) a -> Eff r b
handle h c = continue $ handleRes h $ finish c
