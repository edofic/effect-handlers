-- $desc
-- This module provides a simple exception throwind effect
-- with a single operation `throw` and two ready-made handlers

-- |The functor representing the exception. You shouldn't need
-- to create this manually, just use `throw`.
newtype Exception m a = Exception m deriving (Functor, Typeable)

-- |Throw an exception. The only requirement is that exception
-- be typeable.
throw :: (Member (Exception a) r, Typeable a) => a -> Eff r b
throw m = effect $ \k -> inj $ Exception m

-- |This handler converts an program that might throw an exception
-- into a program that returns either its result or the exception.
exceptionHandler :: Handler (Exception m) r a (Either m a)
exceptionHandler (Left a) = return $ Right a
exceptionHandler (Right (Exception m)) = return $ Left m

-- |This function generates a handler that upon a `throw` short-circuts
-- the computation and returns the default value instead.
defValueExceptionHandler :: a -> Handler (Exception m) r a a
defValueExceptionHandler _ (Left a) = return a
defValueExceptionHandler d (Right (Exception m)) = return d