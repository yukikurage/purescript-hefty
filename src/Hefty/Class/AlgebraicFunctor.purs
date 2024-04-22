module Hefty.Class.Algebraic where

import Prelude

import Data.HFunctor.Variant (VariantH, case_, mapInductive)
import Data.Symbol (class IsSymbol)
import FixFunctor (class HFunctor)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL

class AlgebraicFunctor :: forall k1 k2. ((k2 -> Type) -> k1 -> Type) -> Constraint
class HFunctor h <= AlgebraicFunctor h where
  algebraicCoerce :: forall f g. h f ~> h g

instance (RowToList row rowlist, AlgebraicVariantH rowlist row, HFunctor (VariantH row)) => AlgebraicFunctor (VariantH row) where
  algebraicCoerce = algebraicCoerceVariantH @rowlist @row

class AlgebraicVariantH :: RowList ((Type -> Type) -> Type -> Type) -> Row ((Type -> Type) -> Type -> Type) -> Constraint
class AlgebraicVariantH rowlist row | rowlist -> row where
  algebraicCoerceVariantH :: forall f g. VariantH row f ~> VariantH row g

instance AlgebraicVariantH RL.Nil () where
  algebraicCoerceVariantH = case_

instance (AlgebraicVariantH rowlist row, AlgebraicFunctor h, Cons sym h row row2, IsSymbol sym) => AlgebraicVariantH (RL.Cons sym h rowlist) row2 where
  algebraicCoerceVariantH = mapInductive @sym algebraicCoerce (algebraicCoerceVariantH @rowlist)
