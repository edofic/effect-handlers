{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Dynamic
import Data.Maybe
import Data.Typeable
import Control.Monad.Free
import Control.Applicative
import Unsafe.Coerce

type family If p t f where
  If True t f = t
  If False t f = f

type family Elem x xs where
  Elem x '[] = False
  Elem x (x ': xs) = True
  Elem y (x ': xs) = Elem y xs

type Insert x xs = If (Elem x xs) xs (x ': xs)

type family Merge xs ys where
  Merge '[] ys = ys
  Merge (x ': xs) ys = Insert x (Merge xs ys)

type Member a r = Elem a r ~ True

type family Filter x xs where
  Filter x '[] = '[]
  Filter x (x ': xs) = Filter x xs
  Filter y (x ': xs) = x ': (Filter y xs)

--------------------------------------------

data Union (r :: [* -> *]) (a :: *) where
  Union :: (Functor f, Typeable f) => f a -> Union r a


instance Show (Union fs a) where
  show = const "<union>"

inject :: (Typeable f, Functor f, Member f r) => f a -> Union r a
inject = Union

merge :: (Merge r1 r2 ~ (h ': t)) => Either (Union r1 a) (Union r2 a) -> Union (h ': t) a
merge (Left (Union a)) = Union a
merge (Right (Union a)) = Union a

project :: (Typeable f, Member f r) => Union r a -> Maybe (f a)
project (Union a) = res where
  wantedType = typeOf1 $ fromJust res
  availableType = typeOf1 a
  res = if wantedType == availableType 
        then Just $ unsafeCoerce a 
        else Nothing

split :: (Typeable f, Member f r, r' ~ Filter f r) => Union r a -> Either (Union r' a) (f a)
split u@(Union d) = maybe (Left $ Union d) Right $ project u

trivial :: (Typeable f) => Union '[f] a -> f a
trivial = fromJust . project

instance Functor (Union r) where
  fmap f (Union a) = Union $ fmap f a

-----------------

data Empty

data Ops a = Ask (Int -> a) | Throw String (Empty -> a) deriving Functor

---data Eff e a = Value a | Op (e (Eff e a))

--newtype EffF e a = Op (e (Eff e a)) deriving Functor

newtype Eff e a = Eff { unEff :: Free e a } 
                  deriving (Functor, Applicative, Monad)
{-
program1 :: Eff Ops ()
program1 = do
  v <- Eff $ Free $ Ask return
  Eff $ Free $ Throw (show v) $ undefined
  return ()

program2 :: Eff Ops Int
program2 = do
  v1 <- Eff $ Free $ Ask return
  v2 <- Eff $ Free $ Ask return
  return (v1 + v2)

runOps :: Eff Ops a -> Either String a
runOps (Eff (Pure a)) = Right a
runOps (Eff (Free op)) = runOp op where
  runOp :: Ops (Free Ops a) -> Either String a
  runOp (Ask k) = runOps $ Eff $ k 1
  runOp (Throw msg _) = Left msg
-}
------------------------

data JustAsk a = JustAsk (Int -> a) deriving (Functor, Typeable)
data JustThrow m a = JustThrow m (Empty -> a) deriving (Functor, Typeable)

program3 :: Eff (Union '[JustAsk, JustThrow String]) ()
program3 = do
  v <- Eff $ Free $ inject $ JustAsk return
  Eff $ Free $ inject $ JustThrow (show v) undefined
  return ()

runTrivialOps :: Eff (Union '[]) a -> a
runTrivialOps (Eff (Pure a)) = a

{-}
handle :: (Filter e xs ~ ys) => (Either a (e (Eff (Union ys) b)) -> Eff (Union ys) b) -> Eff (Union xs) a -> Eff (Union ys) b
handle step (Eff (Pure a)) = step $ Left a
handle step (Eff (Free e)) = case split e of
  --Left other -> _1
  Right eff -> _
-}


{-
handleThrow :: (Filter Throw xs ~ ys) => Eff (Union xs) a -> Eff (Union ys) (Either String a)
-}