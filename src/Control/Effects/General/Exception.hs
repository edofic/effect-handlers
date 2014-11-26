newtype Exception m a = Exception m deriving (Functor, Typeable)

throw :: (Member (Exception a) r, Typeable a) => a -> Eff r b
throw m = effect $ \k -> inj $ Exception m

exceptionHandler :: Handler (Exception m) r a (Either m a)
exceptionHandler (Left a) = return $ Right a
exceptionHandler (Right (Exception m)) = return $ Left m

defValueExceptionHandler :: a -> Handler (Exception m) r a a
defValueExceptionHandler _ (Left a) = return a
defValueExceptionHandler d (Right (Exception m)) = return d