module Hefty.Data.Algebraic where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Hefty.Class.AlgFunctor (class AlgFunctor)
import Hefty.Data.HFunctor (class HFunctor)
import Hefty.Data.Handler (HandlerM, handlerM)
import Hefty.Data.Hefty (Hefty)
import Prim.Row (class Cons)

newtype Algebraic :: forall k1 k2. (k1 -> Type) -> k2 -> k1 -> Type
newtype Algebraic f g a = Algebraic (f a)

derive instance Newtype (Algebraic f g a) _
derive newtype instance Functor f => Functor (Algebraic f g)
instance HFunctor (Algebraic f) where
  hmap _ (Algebraic fa) = Algebraic fa

instance AlgFunctor (Algebraic f) where
  algebraicCoerce (Algebraic fa) = Algebraic fa

handlerAlgM :: forall @sym f r1 r2 m. Cons sym (Algebraic f) () r1 => (forall b a. f b -> (b -> Hefty r2 (m a)) -> Hefty r2 (m a)) -> HandlerM r1 r2 m
handlerAlgM f = handlerM @sym \_ hHmB bToHefma -> f (unwrap hHmB) bToHefma