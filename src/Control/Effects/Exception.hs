module Control.Effects.Exception where

import Control.Effects
import Data.Typeable

data Exception m a = Exception m deriving (Functor, Typeable)

throw :: (Member (Exception a) r, Typeable a) => a -> Eff r b
throw = effect . Exception

exceptionHandler :: Handler (Exception m) r a (Either m a)
exceptionHandler (Left a) = return $ Right a
exceptionHandler (Right (Exception m)) = return $ Left m


defValueExeptionHandler :: a -> Handler (Exception m) r a a
defValueExeptionHandler _ (Left a) = return a
defValueExeptionHandler d (Right (Exception m)) = return d
