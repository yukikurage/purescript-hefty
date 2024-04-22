module Hefty.Data.HFunctor where

import Prelude

class HFunctor :: forall k1 k2. ((k1 -> Type) -> k2 -> Type) -> Constraint
class HFunctor h where
  hmap :: forall f g. (f ~> g) -> h f ~> h g
