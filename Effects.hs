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

import Data.Dynamic
import Data.Maybe
import Data.Typeable
import Control.Monad.Free
import Control.Applicative

newtype CoProduct (a :: [*]) = CoProduct { unCPR :: Dynamic }

instance Show (CoProduct a) where
  show _ = "<CoProduct>"

type family If p t f where
  If True t f = t
  If False t f = f

type family Elem x xs where
  Elem x '[] = False
  Elem x (x ': xs) = True
  Elem y (x ': xs) = Elem y xs

type Insert x xs = If (Elem x xs) xs (x ': xs)

type family Union xs ys where
  Union '[] ys = ys
  Union (x ': xs) ys = Insert x (Union xs ys)

type Member a r = Elem a r ~ True

type family Filter x xs where
  Filter x '[] = '[]
  Filter x (x ': xs) = Filter x xs
  Filter y (x ': xs) = x ': (Filter y xs)

inject :: (Typeable a, Member a r) => a -> CoProduct r
inject = CoProduct . toDyn

merge :: (Union r1 r2 ~ (h ': t)) => Either (CoProduct r1) (CoProduct r2) -> CoProduct (h ': t)
merge = CoProduct . either unCPR unCPR

project :: (Typeable a, Member a r) => CoProduct r -> Maybe a
project (CoProduct d) = fromDynamic d

split :: (Typeable a, r' ~ Filter a r) => CoProduct r -> Either (CoProduct r') a
split (CoProduct d) = maybe (Left $ CoProduct d) Right $ fromDynamic d

trivial :: (Typeable a) => CoProduct '[a] -> a
trivial = fromJust . project

-----------------

data Empty

data Ops a = Ask (Int -> a) | Throw String (Empty -> a) deriving Functor

---data Eff e a = Value a | Op (e (Eff e a))

--newtype EffF e a = Op (e (Eff e a)) deriving Functor

newtype Eff e a = Eff { unEff :: Free e a } 
                  deriving (Functor, Applicative, Monad)

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

------------------------

class Functorful r
instance Functorful (CoProduct '[]) a where
instance (Functor f, Functorful r' a) => Functorful (CoProduct ((f a) ': r')) a where


data JustAsk a r = JustAsk (a -> r)
data JustThrow m r = JustThrow m (Empty -> r)

check :: forall (r :: [*]) (a :: *) . Functorful r a => CoProduct r -> CoProduct r
check = id

{-
program3 = do
  v <- Eff $ Free $ ??? inject (JustAsk ???)
  Eff $ Free $ ??? inject (JustThrow (show v) undefined)
  return ()

runTrivialOps :: Eff (CoProduct '[]) a -> a

handleThrow :: (Filter Throw xs ~ ys) => Eff (CoProduct xs) a -> Eff (CoProduct ys) (Either String a)
-}