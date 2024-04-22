module Hefty.Data.Hefty where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Free as Free
import Control.Monad.Rec.Class (class MonadRec)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Hefty.Data.HFunctor.Variant (class HFunctorV, VariantH, case_, hmapV, inj)
import Hefty.Data.HFunctor.Variant as V
import Hefty.Data.Row.Meta (class RowMeta)
import Prim.Row (class Cons, class Union)

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

liftOne :: forall @sym r1 r2 h. IsSymbol sym => Cons sym h r1 r2 => h (Hefty r2) ~> Hefty r2
liftOne h = liftAll $ inj @sym h

mkHeftyAll :: forall r a b. (VariantH r) (Hefty r) b -> (b -> Hefty r a) -> Hefty r a
mkHeftyAll v k = liftAll v >>= k

mkHeftyOne :: forall @sym r1 r h a b. IsSymbol sym => Cons sym h r1 r => h (Hefty r) b -> (b -> Hefty r a) -> Hefty r a
mkHeftyOne v k = mkHeftyAll (inj @sym v) k

wrapAll :: forall r a. (VariantH r) (Hefty r) (Hefty r a) -> Hefty r a
wrapAll h = mkHeftyAll h identity

wrapOne :: forall @sym r1 r h a. IsSymbol sym => Cons sym h r1 r => h (Hefty r) (Hefty r a) -> Hefty r a
wrapOne h = wrapAll (inj @sym h)

extractAll :: forall a. Hefty () a -> a
extractAll h = resumeHefty (\v _ -> case_ v) identity h

expandOne :: forall @sym h r1 r2. IsSymbol sym => HFunctorV r1 => Cons sym h r1 r2 => Hefty r1 ~> Hefty r2
expandOne h = hoistHeftyAll (V.expandOne @sym) h

expandRight :: forall @r1 @r2 @r3. Union r1 r2 r3 => HFunctorV r1 => Hefty r1 ~> Hefty r3
expandRight h = hoistHeftyAll (V.expandRight @r1 @r2) h

expandLeft :: forall @r1 @r2 @r3 a. HFunctorV r2 => Union r1 r2 r3 => RowMeta r1 => Hefty r2 a -> Hefty r3 a
expandLeft h = hoistHeftyAll (V.expandLeft @r1 @r2) h

expand
  :: forall @r1 @r2 @r3 @r4 r5
   . Union r2 r3 r4
  => Union r1 r4 r5
  => RowMeta r1
  => HFunctorV r2
  => Hefty r2 ~> Hefty r5
expand h = hoistHeftyAll (V.expand @r1 @r2 @r3) h

resumeHefty :: forall ops a r. (forall b. VariantH ops (Hefty ops) b -> (b -> Hefty ops a) -> r) -> (a -> r) -> Hefty ops a -> r
resumeHefty f g freeH = Free.resume' op g $ unHefty freeH
  where
  op :: forall b. VariantH ops (Hefty ops) b -> (b -> Free ((VariantH ops) (Hefty ops)) a) -> r
  op ext next = f ext (Hefty <<< next)

handleHeftyAll :: forall ops a m. Applicative m => HFunctorV ops => (forall b x. VariantH ops m b -> (b -> m x) -> m x) -> Hefty ops a -> m a
handleHeftyAll f = resumeHefty alg pure
  where
  alg :: forall b. VariantH ops (Hefty ops) b -> (b -> Hefty ops a) -> m a
  alg ext next = f (hmapV (handleHeftyAll f) ext) (\b -> handleHeftyAll f $ next b)

foldHefty :: forall r m. HFunctorV r => MonadRec m => (VariantH r m ~> m) -> Hefty r ~> m
foldHefty alg (Hefty free) = Free.foldFree alg' free
  where
  alg' :: VariantH r (Hefty r) ~> m
  alg' acc = alg $ hmapV (foldHefty alg) acc

hoistHeftyAll :: forall r1 r2. HFunctorV r1 => (forall f. VariantH r1 f ~> VariantH r2 f) -> Hefty r1 ~> Hefty r2
hoistHeftyAll f (Hefty free) = Hefty $ Free.hoistFree g free
  where
  g :: VariantH r1 (Hefty r1) ~> VariantH r2 (Hefty r2)
  g kk = f $ hmapV (hoistHeftyAll f) kk

hoistHeftyOne
  :: forall @sym r1 r2 r3 h1 h2
   . IsSymbol sym
  => Cons sym h1 r3 r1
  => Cons sym h2 r3 r2
  => HFunctorV r1
  => (forall f. h1 f ~> h2 f)
  -> Hefty r1 ~> Hefty r2
hoistHeftyOne f = hoistHeftyAll (V.mapInductive @sym f identity)
