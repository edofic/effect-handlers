{-# LANGUAGE OverlappingInstances #-}

-- |This module provides an open union of functors.
module Data.Union 
( Union
, Member
, inj
, prj
, decomp
, trivial
) where

import Data.Maybe
import Data.Typeable
import Unsafe.Coerce (unsafeCoerce)

-- |`Union` is an open sum of functors
-- A value of type `Union` r a is a value f a for some f that is a member of the r list
-- Since direct construction is not safe you have to use `inj` to create a value.
data Union (r :: [* -> *]) (a :: *) where
  Union :: (Functor f, Typeable f) => f a -> Union r a

instance Functor (Union r) where
  fmap f (Union fa) = Union (fmap f fa)

-- |The `Member` type clas denotes that f is a member of type list r
class Member (f :: * -> *) (r :: [* -> *]) where
instance Member h (h ': t)
instance (Member x t) => Member x (h ': t)

-- |Smart constructor for `Union`. Injects the functor into any union
-- of which the said functor is a member. Please note that only the 
-- type constructor need be a `Typeable`.
inj :: (Typeable f, Functor f, Member f r) => f a -> Union r a
inj = Union 

-- |Project a `Union` into a specific functor.
prj :: (Typeable f, Member f r) => Union r a -> Maybe (f a)
prj (Union d) = res where
  availableType = typeOf1 d
  wantedType = typeOf1 $ fromJust res
  res = if availableType == wantedType
        then Just $ unsafeCoerce d
        else Nothing

-- |Decompose a `Union`. Similar to `prj` but gives you a
-- `Union` instance without the functor f in type if projection fails.
decomp :: (Typeable f) => Union (f ': r) a -> Either (f a) (Union r a)
decomp u@(Union d) = maybe (Right $ Union d) Left $ prj u

-- |A `Union` of one functor can only be that. Safe cast.
trivial :: (Typeable f) => Union '[f] a -> f a
trivial = fromJust . prj
