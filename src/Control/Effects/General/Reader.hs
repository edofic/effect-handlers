-- $desc
-- This module provides the familiar reader effect.

-- |The functor representing the effect. You shouldn't need
-- to create this manually, just use `ask` or `reader`
newtype Reader w a = Reader (w -> a) deriving (Functor, Typeable)

-- |Get the value from the reader
ask :: (Member (Reader a) r, Typeable a) => Eff r a
ask = effect $ \k -> inj $ Reader k

-- |Lift a function into a reader.
reader :: (Member (Reader a) r, Typeable a) => (a -> b) -> Eff r b
reader f = effect $ \k -> inj $ Reader $ k . f

-- |The obvious handler that just embeds the value provided.
readerHandler :: w -> Handler (Reader w) r a a
readerHandler _ (Left a) = return a
readerHandler n (Right (Reader k)) = k n