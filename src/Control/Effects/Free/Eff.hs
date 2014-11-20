module Control.Effects.Free.Eff
( Eff
, Handler
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

type Res r = Free (Union r)

newtype Eff r a  = Eff { unEff :: Res r a } deriving (Functor, Applicative, Monad)

type Handler e r a b = Either a (e (Res r b)) -> Res r b

effect :: (forall b . (a -> Res r b) -> Union r (Res r b)) -> Eff r a
effect e = Eff $ Free $ e return 

runPure :: Eff '[] a -> a
runPure (Eff (Pure a)) = a

runPureRes :: Res '[] a -> a
runPureRes (Pure a) = a

finish :: Eff r a -> Res r a
finish = unEff

continue :: Res r a -> Eff r a
continue = Eff

handle :: (Functor e, Typeable e) => Handler e r a b -> Eff (e ': r) a -> Eff r b
handle f (Eff (Pure a)) = Eff $ f $ Left a
handle f (Eff (Free u)) = case decomp (Eff `fmap` u) of 
  Left a -> Eff $ f $ Right $ (unEff . handle f) `fmap` a 
  Right a -> Eff $ Free $ (unEff . handle f) `fmap` a
