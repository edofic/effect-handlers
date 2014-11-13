{-# LANGUAGE OverlappingInstances #-}

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

---- open ended uion ----
data Union (r :: [* -> *]) (a :: *) where
  Union :: (Functor f, Typeable f) => f a -> Union r a

instance Functor (Union r) where
  fmap f (Union fa) = Union (fmap f fa)

class Member (f :: * -> *) (r :: [* -> *]) where
instance Member h (h ': t)
instance (Member x t) => Member x (h ': t)

inj :: (Typeable f, Functor f, Member f r) => f a -> Union r a
inj = Union 

prj :: (Typeable f, Member f r) => Union r a -> Maybe (f a)
prj (Union d) = res where
  availableType = typeOf1 d
  wantedType = typeOf1 $ fromJust res
  res = if availableType == wantedType
        then Just $ unsafeCoerce d
        else Nothing

decomp :: (Typeable f) => Union (f ': r) a -> Either (f a) (Union r a)
decomp u@(Union d) = maybe (Right $ Union d) Left $ prj u

trivial :: (Typeable f) => Union '[f] a -> f a
trivial = fromJust . prj
