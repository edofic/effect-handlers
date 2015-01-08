-- |This is the simplest implementation of the effect monad
-- directly using a `Free` over a `Union` of functors
-- representing effects
module Control.Effects.Free.Eff
( Eff
, Handler
, Comp (Value, Comp)
, effect
, runPure
, runPureRes
, finish
, continue
, handle
, inj
, Member
, Typeable
) where

import Control.Monad.Free
import Control.Applicative
import Data.Union
import Data.Typeable

-- |Result structure of the program is directly `Free` over `Union` 
-- indexed by the list of effect functors.
type Res r = Free (Union r)

-- |The type of programs using effects. Indexed by a type list of 
-- functors representing effects. This implementation is just a wrapper
-- for the result type.
newtype Eff r a  = Eff { unEff :: Res r a } deriving (Functor, Applicative, Monad)

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
effect e = Eff $ Free $ e return 

-- |A program without effects is guaranteed to be pure so you 
-- can safely convert it into a value.
runPure :: Eff '[] a -> a
runPure (Eff (Pure a)) = a

-- |Like `runPure` but for program results. You only need this for implementing
-- some handlers. 
runPureRes :: Res '[] a -> a
runPureRes (Pure a) = a

-- |Finish a program and convert it into a result structure. Free at runtime.
finish :: Eff r a -> Res r a
finish = unEff

-- |Convert a result back into a program in order to compose it. Free at runtime.
continue :: Res r a -> Eff r a
continue = Eff

-- |Use a `Handler` on an `Eff` program to stripe away the first layer of effects.
-- There are some issues if you are using a handler that is somewhat polymorphic in `e`
-- As the compiler cannot figure out which effect are you handling. Currently the best 
-- solution seems to be to manually specify type of the handler such that it is monomorphic
-- in `e`. Sorry.
handle :: (Functor e, Typeable e) => Handler e r a b -> Eff (e ': r) a -> Eff r b
handle f (Eff (Pure a)) = Eff $ f $ Value a
handle f (Eff (Free u)) = case decomp (Eff `fmap` u) of 
  Left a -> Eff $ f $ Comp $ (unEff . handle f) `fmap` a 
  Right a -> Eff $ Free $ (unEff . handle f) `fmap` a
