module Hefty.Data.Effect.Aff where

import Prelude

import Effect.Aff (Aff)
import Hefty.Data.Algebraic (Algebraic(..))
import Hefty.Data.HFunctor.Variant (fromSingleton)
import Hefty.Data.Hefty (Hefty, foldHefty, liftOne)
import Type.Row (type (+))

newtype AffOp a = AffOp (Aff a)

type Aff_ :: Symbol
type Aff_ = "aff"

type AFF :: forall k2. Row (k2 -> Type -> Type) -> Row (k2 -> Type -> Type)
type AFF r = (aff :: Algebraic AffOp | r)

liftAff :: forall r a. Aff a -> Hefty (AFF + r) a
liftAff aff = liftOne @Aff_ $ Algebraic $ AffOp aff

runBaseAff :: Hefty (AFF + ()) ~> Aff
runBaseAff = foldHefty
  ( \vm -> case fromSingleton @Aff_ vm of
      (Algebraic (AffOp aff)) -> aff
  )
