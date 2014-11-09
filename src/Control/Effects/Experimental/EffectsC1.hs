module Control.Effects.Experimental.EffectsC1 where

import Control.Applicative
import Control.Effects (Union, Member, inj, prj, decomp, trivial)
import Control.Monad.Cont 
import Control.Monad.Free
import Data.Typeable

data Res r a = Val a | E (Union r (Res r a))
             deriving Functor 

instance Applicative (Res r) where
  pure = Val
  mf <*> ma = mf >>= flip fmap ma

instance Monad (Res r) where
  return = Val
  (Val a) >>= f = f a
  (E u) >>= f = E $ fmap (>>= f) u

newtype Eff r a = Eff { runEff :: forall b . (a -> Res r b) -> Res r b}

instance Functor (Eff r) where
  fmap f eff = Eff $ \k -> runEff eff (k . f)

instance Applicative (Eff r) where
  pure a = Eff ($a)
  mf <*> mx = Eff $ \k -> 
              runEff mf $ \f ->
              runEff mx $ k . f

instance Monad (Eff r) where
  return a = Eff ($a)
  m >>= f = Eff $ \k -> 
            runEff m $ \a ->
            runEff (f a ) k

type Handler e r a b = Either a (e (Res r b)) -> Res r b

liftEffect :: (forall b . (a -> Res r b) -> Union r (Res r b)) -> Eff r a
liftEffect e = Eff $ \k -> E $ e k

runPure :: Eff '[] a -> a
runPure e = a where
  Val a = runEff e Val

finish :: Eff r a -> Res r a
finish c = runEff c Val

continue :: Res r a -> Eff r a
continue r = Eff (r >>=)


handleRes :: (Functor e, Typeable e) => Handler e r a b -> Res (e ': r) a -> Res r b
handleRes h (Val a) = h $ Left a
handleRes h (E u) = case decomp u of 
  Left  u -> h $ Right $ fmap (handleRes h) u
  Right u -> E $ fmap (handleRes h) u

handle :: (Functor e, Typeable e) => Handler e r a b -> Eff (e ': r) a -> Eff r b
handle h c = continue $ handleRes h $ finish c


---------------------------------------------

newtype Reader w a = Reader (w -> a) deriving (Functor, Typeable)

ask :: (Member (Reader a) r, Typeable a) => Eff r a
ask = liftEffect $ \k -> inj $ Reader k

reader :: (Member (Reader a) r, Typeable a) => (a -> b) -> Eff r b
reader f = liftEffect $ \k -> inj $ Reader $ k . f

readerHandler :: w -> Handler (Reader w) r a a
readerHandler _ (Left a) = return a
readerHandler n (Right (Reader k)) = k n

newtype Exception m a = Exception m deriving (Functor, Typeable)

throw :: (Member (Exception a) r, Typeable a) => a -> Eff r b
throw m = liftEffect $ \k -> inj $ Exception m

exceptionHandler :: Handler (Exception m) r a (Either m a)
exceptionHandler (Left a) = return $ Right a
exceptionHandler (Right (Exception m)) = return $ Left m

program3 = do
  v <- ask
  if v < 15 
  then throw $ show v
  else return (v+1)

program3run n  = runPure
                  . handle exceptionHandler 
                  . handle (readerHandler n)

program3res :: Integer -> Either String Integer
program3res n = program3run n program3


