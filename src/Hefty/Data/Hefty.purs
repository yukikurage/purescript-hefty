module Hefty.Data.Hefty where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Free as Free
import Control.Monad.Rec.Class (class MonadRec)
import Data.HFunctor.Variant (VariantH, case_, inj)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import FixFunctor (class HFunctor)
import Prim.Row (class Cons)

-- | https://dl.acm.org/doi/abs/10.1145/3571255
-- | https://www.sciencedirect.com/science/article/pii/S0167642324000091
newtype Hefty ops a = Hefty (Free (VariantH ops (Hefty ops)) a)

unHefty :: forall h a. Hefty h a -> Free (VariantH h (Hefty h)) a
unHefty (Hefty f) = f

derive instance Newtype (Hefty h a) _
derive newtype instance Functor (Hefty h)
derive newtype instance Apply (Hefty h)
derive newtype instance Applicative (Hefty h)
derive newtype instance Bind (Hefty h)
derive newtype instance Monad (Hefty h)
derive newtype instance MonadRec (Hefty h)

liftAll :: forall r. (VariantH r) (Hefty r) ~> Hefty r
liftAll v = Hefty $ Free.liftF v

lift :: forall @sym r1 r2 h. IsSymbol sym => Cons sym h r1 r2 => HFunctor (VariantH r2) => h (Hefty r2) ~> Hefty r2
lift h = liftAll $ inj @sym h

mkHefty :: forall r a b. (VariantH r) (Hefty r) b -> (b -> Hefty r a) -> Hefty r a
mkHefty v k = liftAll v >>= k

mkHeftyOne :: forall @sym r1 r h a b. IsSymbol sym => Cons sym h r1 r => h (Hefty r) b -> (b -> Hefty r a) -> Hefty r a
mkHeftyOne v k = mkHefty (inj @sym v) k

wrapAll :: forall r a. (VariantH r) (Hefty r) (Hefty r a) -> Hefty r a
wrapAll h = mkHefty h identity

wrapOne :: forall @sym r1 r h a. IsSymbol sym => Cons sym h r1 r => h (Hefty r) (Hefty r a) -> Hefty r a
wrapOne h = wrapAll (inj @sym h)

extract :: forall a. Hefty () a -> a
extract h = resumeHefty' (\v _ -> case_ v) identity h

resumeHefty' :: forall ops a r. HFunctor (VariantH ops) => (forall b. VariantH ops (Hefty ops) b -> (b -> Hefty ops a) -> r) -> (a -> r) -> Hefty ops a -> r
resumeHefty' f g freeH = Free.resume' op g $ unHefty freeH
  where
  op :: forall b. VariantH ops (Hefty ops) b -> (b -> Free ((VariantH ops) (Hefty ops)) a) -> r
  op ext next = f ext (Hefty <<< next)
