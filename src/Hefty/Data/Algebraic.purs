module Hefty.Data.Algebraic where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Hefty.Class.AlgFunctor (class AlgFunctor)
import Hefty.Data.HFunctor (class HFunctor)
import Hefty.Data.Handler (Handler, HandlerM, mkHandler, mkHandler', mkHandlerM)
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

mkHandlerAlgM :: forall @sym f r1 r2 m. Cons sym (Algebraic f) () r1 => (forall b a. f b -> (b -> Hefty r2 (m a)) -> Hefty r2 (m a)) -> HandlerM r1 r2 m
mkHandlerAlgM f = mkHandlerM @sym \hHefB bToHefMa -> f (unwrap hHefB) bToHefMa

mkHandlerAlg :: forall @sym f r1 r2. Cons sym (Algebraic f) () r1 => (forall b a. f b -> (b -> Hefty r2 a) -> Hefty r2 a) -> Handler r1 r2
mkHandlerAlg f = mkHandler @sym \hHefB bToHefMa -> f (unwrap hHefB) bToHefMa

mkHandlerAlg' :: forall @sym f r1 r2. Cons sym (Algebraic f) () r1 => (f ~> Hefty r2) -> Handler r1 r2
mkHandlerAlg' f = mkHandler' @sym \hHefB -> f (unwrap hHefB)
