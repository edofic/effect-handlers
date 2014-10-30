{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Effects where

import Control.Monad.Free
import Control.Applicative
import Data.Maybe
import Data.Typeable
import Unsafe.Coerce

---- open ended uion ----
data Union (r :: [* -> *]) (a :: *) where
  Union :: (Functor f, Typeable f) => f a -> Union r a

instance Show (Union r a) where
  show _ = "<union>"

instance Functor (Union r) where
  fmap f (Union fa) = Union (fmap f fa)

-- type family HasMember f r where
--   HasMember x (x ': xs) = ()
--   HasMember x (y ': xs) = HasMember x xs
--type Member f r = HasMember f r ~ ()

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


---- examples ----
data JustAsk a = JustAsk (Int -> a) deriving (Functor, Typeable)

ask :: Member JustAsk r => Eff r Int
ask = effect $ JustAsk return

askHandler :: Int -> Handler JustAsk r a a
askHandler _ (Left a) = return a
askHandler n (Right (JustAsk k)) = k n

data JustThrow m a = JustThrow m deriving (Functor, Typeable)

throw :: (Member (JustThrow a) r, Typeable a) => a -> Eff r b
throw = effect . JustThrow

throwHandler :: Handler (JustThrow m) r a (Either m a)
throwHandler (Left a) = return $ Right a
throwHandler (Right (JustThrow m)) = return $ Left m

--m :: Eff '[JustAsk, JustThrow String] ()
program3 = do
  v <- ask
  throw $ show v
  return ()

handleProgram3 n = runEff . handle throwHandler . handle (askHandler n)
