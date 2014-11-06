module Control.Effects.Exception where

import Control.Effects
import Data.Typeable

data Exception m a = Exception m deriving (Functor, Typeable)

throw :: (Member (Exception a) r, Typeable a) => a -> Eff r b
throw = effect . Exception

exceptionHandler :: Handler (Exception m) r a (Either m a)
exceptionHandler (Left a) = return $ Right a
exceptionHandler (Right (Exception m)) = return $ Left m