module Control.Effects.Cont.Writer where

import Control.Effects.Cont.Eff
import Data.Monoid

data Writer m a = Writer m a deriving (Functor, Typeable)

tell :: (Member (Writer m) r, Typeable m) => m -> Eff r ()
tell m = effect $ \k -> inj $ Writer m $ k () 

writerHandler :: (Monoid m) => Handler (Writer m) r a (a, m)
writerHandler (Left a) = return (a, mempty)
writerHandler (Right (Writer m k)) = do
  (a, m') <- k
  return (a, m <> m')
