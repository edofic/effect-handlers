{-# LANGUAGE OverlappingInstances #-}

module Control.Effects 
( Union
, Member
, inj
, prj
, decomp
, trivial
, Eff
, Handler
, effect
, runEff
, handle
) where

import Control.Monad.Free
import Control.Applicative
import Data.Maybe
import Data.Typeable
import Unsafe.Coerce

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


---- effect helpers ----
newtype Eff r a  = Eff { unEff :: Free (Union r) a } deriving (Functor, Applicative, Monad)
type Handler e r a b = Either a (e (Eff r b)) -> Eff r b

effect :: (Functor f, Member f r, Typeable f) => f (Eff r a) -> Eff r a
effect = Eff . Free . inj . fmap unEff 

runEff :: Eff '[] a -> a
runEff (Eff (Pure a)) = a

handle :: (Functor e, Typeable e) => Handler e r a b -> Eff (e ': r) a -> Eff r b
handle f (Eff (Pure a)) = f (Left a)
handle f (Eff (Free u)) = either 
    (f . Right . fmap (handle f))
    (Eff . Free . (fmap unEff) . fmap (handle f)) 
  (decomp $ fmap Eff u)

