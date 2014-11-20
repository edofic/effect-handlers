newtype Reader w a = Reader (w -> a) deriving (Functor, Typeable)

ask :: (Member (Reader a) r, Typeable a) => Eff r a
ask = effect $ \k -> inj $ Reader k

reader :: (Member (Reader a) r, Typeable a) => (a -> b) -> Eff r b
reader f = effect $ \k -> inj $ Reader $ k . f

readerHandler :: w -> Handler (Reader w) r a a
readerHandler _ (Left a) = return a
readerHandler n (Right (Reader k)) = k n