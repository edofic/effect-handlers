import Control.Monad

data State s a = SGet (s -> a) 
               | SPut s a 
               deriving (Functor, Typeable)

get :: (Member (State a) r, Typeable a) => Eff r a
get = effect $ \k -> inj $ SGet k

put :: (Member (State s) r, Typeable s) => s -> Eff r ()
put a = effect $ \k -> inj $ SPut a $ k ()

state :: (Member (State s) r, Typeable s) => (s -> (a, s)) -> Eff r a
state f = do
  s <- get
  let (a, s') = f s
  put s'
  return a

stateHandler :: Handler (State s) r a (s -> Eff r a) 
stateHandler (Left a) = return $ const $ return a
stateHandler (Right (SGet k)) = return $ \s -> join $ ($s) `fmap` continue (k s)
stateHandler (Right (SPut s k)) = return $ \_ -> join $ continue $ fmap ($s) k 
