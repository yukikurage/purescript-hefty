module Hefty.Data.FreeH where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Free as Free
import Control.Monad.Rec.Class (class MonadRec)
import Data.HFunctor.Variant (VariantH)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import FixFunctor (class HFunctor, hmap)

newtype FreeH h a = FreeH (Free (h (FreeH h)) a)

derive instance Newtype (FreeH h a) _
derive newtype instance Functor (FreeH h)
derive newtype instance Apply (FreeH h)
derive newtype instance Applicative (FreeH h)
derive newtype instance Bind (FreeH h)
derive newtype instance Monad (FreeH h)
derive newtype instance MonadRec (FreeH h)

hoistFreeH :: forall h1 h2. HFunctor h1 => (forall f. h1 f ~> h2 f) -> FreeH h1 ~> FreeH h2
hoistFreeH f (FreeH free) = FreeH $ Free.hoistFree g free
  where
  g :: h1 (FreeH h1) ~> h2 (FreeH h2)
  g kk = f $ hmap (hoistFreeH f) kk

foldFreeH :: forall h m. HFunctor h => MonadRec m => (h m ~> m) -> FreeH h ~> m
foldFreeH alg (FreeH free) = Free.foldFree alg' free
  where
  alg' :: h (FreeH h) ~> m
  alg' acc = alg $ hmap (foldFreeH alg) acc

substFreeH :: forall h l. HFunctor h => (h (FreeH l) ~> (FreeH l)) -> FreeH h ~> FreeH l
substFreeH = foldFreeH

resumeFreeH' :: forall h a r. HFunctor h => (forall b. h (FreeH h) b -> (b -> FreeH h a) -> r) -> (a -> r) -> FreeH h a -> r
resumeFreeH' f g (FreeH free) = Free.resume' op g free
  where
  op :: forall b. h (FreeH h) b -> (b -> Free (h (FreeH h)) a) -> r
  op ext next = f ext (FreeH <<< next)

liftFreeH :: forall h. h (FreeH h) ~> FreeH h
liftFreeH op = FreeH $ Free.liftF op

wrapFreeH :: forall h a. h (FreeH h) (FreeH h a) -> FreeH h a
wrapFreeH op = mkFreeH op identity

mkFreeH :: forall h a b. h (FreeH h) b -> (b -> FreeH h a) -> FreeH h a
mkFreeH op next = join $ map next $ liftFreeH op
