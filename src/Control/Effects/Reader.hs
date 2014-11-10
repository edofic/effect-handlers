module Control.Effects.Reader where

import Control.Effects.Class hiding (return, (>>=))
import Control.Effects.Union
import Data.Typeable

newtype Reader w a = Reader (w -> a) deriving (Functor, Typeable)

ask :: (Member (Reader a) r, Typeable a, MonadEffect m) => m r a
ask = effect $ \k -> Reader k

reader :: (Member (Reader a) r, Typeable a, MonadEffect m) => (a -> b) -> m r b
reader f = effect $ \k -> Reader $ k . f

readerHandler :: w -> Handler (Reader w) r a a
readerHandler _ (Left a) = return a
readerHandler n (Right (Reader k)) = k n
