module Hefty.Class.AlgFunctor where

import Prelude

import Data.Symbol (class IsSymbol)
import Hefty.Data.HFunctor (class HFunctor)
import Hefty.Data.HFunctor.Variant (VariantH, case_, mapInductive)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL

class AlgFunctor :: forall k1 k2. ((k2 -> Type) -> k1 -> Type) -> Constraint
class HFunctor h <= AlgFunctor h where
  algebraicCoerce :: forall f g. h f ~> h g

instance (RowToList row rowlist, AlgebraicVariantH rowlist row, HFunctor (VariantH row)) => AlgFunctor (VariantH row) where
  algebraicCoerce = algebraicCoerceVariantH @rowlist @row

class AlgebraicVariantH :: RowList ((Type -> Type) -> Type -> Type) -> Row ((Type -> Type) -> Type -> Type) -> Constraint
class AlgebraicVariantH rowlist row | rowlist -> row where
  algebraicCoerceVariantH :: forall f g. VariantH row f ~> VariantH row g

instance AlgebraicVariantH RL.Nil () where
  algebraicCoerceVariantH = case_

instance (AlgebraicVariantH rowlist row, AlgFunctor h, Cons sym h row row2, IsSymbol sym) => AlgebraicVariantH (RL.Cons sym h rowlist) row2 where
  algebraicCoerceVariantH = mapInductive @sym algebraicCoerce (algebraicCoerceVariantH @rowlist)

class (AlgFunctor (VariantH row)) <= AlgFunctorV row where
  algebraicCoerceV :: forall f g. VariantH row f ~> VariantH row g

instance (AlgFunctor (VariantH row)) => AlgFunctorV row where
  algebraicCoerceV = algebraicCoerce
