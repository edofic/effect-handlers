{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |This module provides an open union of functors.
module Data.Union
( Union
, Member
, inj
, prj
, decomp
, trivial
) where

import Unsafe.Coerce (unsafeCoerce)

data Nat = Zero | Succ Nat

data Proxy (n :: Nat) = Proxy

class ReifyNat (n :: Nat) where
  reifyNat :: Proxy n -> Int -> Int

instance ReifyNat Zero where
  reifyNat _ acc = acc

instance (ReifyNat n) => ReifyNat (Succ n) where
  reifyNat _ acc = reifyNat (Proxy :: Proxy n) (acc+1)

type family Index (a :: k) (as :: [k]) :: Nat where
  Index a (a ': t) = Zero
  Index a (b ': t) = Succ (Index a t)

type family DropAt (n :: Nat) (as :: [k]) :: [k] where
  DropAt Zero (a ': as) = as
  DropAt (Succ n) (a ': as) = a ': DropAt n as


-- |`Union` is an open sum of functors
-- A value of type `Union` r a is a value f a for some f that is a member of the r list
-- Since direct construction is not safe you have to use `inj` to create a value.
data Union (fs :: [* -> *]) a where
  At :: Functor f => Int -> f a -> Union s a

instance Functor (Union fs) where
  fmap f (At i fa) = At i (fmap f fa)


-- |The `Member` type denotes that f is a member of type list r
type Member a s = ReifyNat (Index a s)

-- |Smart constructor for `Union`. Injects the functor into any union
-- of which the said functor is a member.
inj :: (Functor f, Member f s) => f a -> Union s a
inj = go Proxy  where
  go :: (Functor f, Member f s) => Proxy (Index f s) -> f a -> Union s a
  go p fa = At (reifyNat p 0) fa


-- |Project a `Union` into a specific functor.
prj :: (Member f s) => Union s a -> Maybe (f a)
prj = go Proxy where
  go :: (Member f s) => Proxy (Index f s) -> Union s a -> Maybe (f a)
  go p (At i a) | reifyNat p 0 == i = Just (unsafeCoerce a)
                | otherwise         = Nothing

-- |Decompose a `Union`. Similar to `prj` but gives you a
-- `Union` instance without the functor f in type if projection fails.
decomp :: Union (f ': s) a -> Either (Union s a) (f a)
decomp (At 0 a) = Right (unsafeCoerce a)
decomp (At i a) = Left (At (i-1) a)

-- |A `Union` of one functor can only be that. Safe cast.
trivial :: Union '[f] a -> f a
trivial (At _ fa) = unsafeCoerce fa
