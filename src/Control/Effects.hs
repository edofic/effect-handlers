{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Effects 
( Union
, Member
, inj
, prj
, decomp
, trivial
, Eff
, Handler
, Rejiggle
, effect
, runEff
, handle
, rejiggle
) where

import Control.Monad.Free
import Control.Applicative
import Data.Maybe
import Data.Typeable
import Unsafe.Coerce
import GHC.Exts (Constraint)

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

type family Rejiggle (r :: [* -> *]) (xs :: [* -> *]) :: Constraint 
type instance Rejiggle r '[]       = ()
type instance Rejiggle r (x ': xs) = (Member x r, Rejiggle r xs)

rejiggle :: (Rejiggle r' r) => Eff r a -> Eff r' a
rejiggle = unsafeCoerce

---- effect helpers ----
type Eff r = Free (Union r)
type Handler e r a b = Either a (e (Eff r b)) -> Eff r b

effect :: (Functor f, Member f r, Typeable f) => f (Eff r a) -> Eff r a
effect = Free . inj

runEff :: Eff '[] a -> a
runEff (Pure a) = a

handle :: (Functor e, Typeable e) => Handler e r a b -> Eff (e ': r) a -> Eff r b
handle f (Pure a) = f (Left a)
handle f (Free u) = either 
 (f . Right . fmap (handle f))
 (Free . fmap (handle f)) 
 (decomp u) 