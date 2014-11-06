module Control.Effects.Reader where

import Control.Effects
import Data.Typeable

data Reader w a = Reader (w -> a) deriving (Functor, Typeable)

ask :: (Member (Reader a) r, Typeable a) => Eff r a
ask = effect $ Reader return

readerHandler :: w -> Handler (Reader w) r a a
readerHandler _ (Left a) = return a
readerHandler n (Right (Reader k)) = k n
