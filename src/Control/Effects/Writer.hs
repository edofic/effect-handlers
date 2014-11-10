module Control.Effects.Writer where

import Control.Effects hiding (return, (>>=))
import Data.Monoid
import Data.Typeable

data Writer m a = Writer m (m -> a) deriving (Functor, Typeable)

tell :: (Member (Writer w) r, Typeable w, MonadEffect m) => w -> m r ()
tell m = effect $ \k -> Writer m $ const $ k ()

listen :: (Member (Writer w) r, Typeable w, Monoid w, MonadEffect m) => m r w
listen = effect $ \k -> Writer mempty k

writerHandler :: (Monoid w) => Handler (Writer w) r a (a, w)
writerHandler (Left a) = 
  return (a, mempty)
writerHandler (Right (Writer m k)) = do
  (a, m') <- k m
  return (a, m <> m')