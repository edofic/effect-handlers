module Control.Effects.Free.Eff
( Eff
, Handler
, effect
, runEff
, handle
, Member
, Typeable
) where

import Control.Monad.Free
import Control.Applicative
import Data.Union
import Data.Typeable

---- effect helpers ----
newtype Eff r a  = Eff { unEff :: Free (Union r) a } deriving (Functor, Applicative, Monad)
type Handler e r a b = Either a (e (Eff r b)) -> Eff r b

effect :: (Functor f, Member f r, Typeable f) => f (Eff r a) -> Eff r a
effect = Eff . Free . inj . fmap unEff 

runEff :: Eff '[] a -> a
runEff (Eff (Pure a)) = a

handle :: (Functor e, Typeable e) => Handler e r a b -> Eff (e ': r) a -> Eff r b
handle f (Eff (Pure a)) = f (Left a)
handle f (Eff (Free u)) = either 
    (f . Right . fmap (handle f))
    (Eff . Free . (fmap unEff) . fmap (handle f)) 
  (decomp $ fmap Eff u)

