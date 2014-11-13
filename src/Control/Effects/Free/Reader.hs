module Control.Effects.Free.Reader where

import Control.Effects.Free.Eff

newtype Reader w a = Reader (w -> a) deriving (Functor, Typeable)

ask :: (Member (Reader a) r, Typeable a) => Eff r a
ask = effect $ Reader return

reader :: (Member (Reader a) r, Typeable a) => (a -> b) -> Eff r b
reader f = effect $ Reader $ return . f

readerHandler :: w -> Handler (Reader w) r a a
readerHandler _ (Left a) = return a
readerHandler n (Right (Reader k)) = k n
