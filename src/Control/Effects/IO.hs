module Control.Effects.IO where 

import Control.Effects hiding (return, (>>=))
import Data.Typeable

data LiftIO a = forall r . LiftIO (IO r) (r -> a) deriving (Typeable)

instance Functor LiftIO where
  fmap f (LiftIO c k) = LiftIO c (f . k)

liftIO :: Member LiftIO r => IO a -> Eff r a
liftIO c = effect $ \k -> LiftIO c k

ioHandler :: Handler LiftIO '[] a (IO a)
ioHandler (Left a) = 
  return $ return a
ioHandler (Right (LiftIO c k)) = 
  return $ c >>= (runPureRes . k)
