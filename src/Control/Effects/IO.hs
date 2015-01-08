-- This module provides a mechanism to embed `IO` computations
-- into the effect monad. Note that the `IO` can only ever be at 
-- the base of your stack (for the same reasons as with transformers).
module Control.Effects.IO where

import Control.Effects.Eff


-- |The functor representing the effect. You shouldn't need
-- to create this manually, just use `liftIO`.
data LiftIO a = forall r . LiftIO (IO r) (r -> a) deriving (Typeable)

instance Functor LiftIO where
  fmap f (LiftIO c k) = LiftIO c (f . k)

-- |Lift an existing `IO` action into the effect monad.
liftIO :: Member LiftIO r => IO a -> Eff r a
liftIO c = effect $ \k -> inj $ LiftIO c k

-- |Handle by converting back to `IO`. Note that it is required that 
-- the effect stack is otherwise empty - this handler would not
-- typecheck otherwise.
ioHandler :: Handler LiftIO '[] a (IO a)
ioHandler (Value a) = 
  return $ return a
ioHandler (Comp (LiftIO c k)) = 
  return $ c >>= (runPureRes . k)
