module Control.Effects.Writer where

import Control.Effects
import Data.Monoid
import Data.Typeable

data Writer m a = Writer m (m -> a) deriving (Functor, Typeable)

tell :: (Member (Writer m) r, Typeable m) => m -> Eff r ()
tell m = effect $ Writer m $ const $ return ()

listen :: (Member (Writer m) r, Typeable m, Monoid m) => Eff r m
listen = effect $ Writer mempty return

writerHandler :: (Monoid m) => Handler (Writer m) r a (a, m)
writerHandler (Left a) = 
  return (a, mempty)
writerHandler (Right (Writer m k)) = do
  (a, m') <- k m
  return (a, m <> m')