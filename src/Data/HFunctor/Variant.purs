module Data.HFunctor.Variant (VariantH, class HFunctorVariantH, hmapVariantH, inj, takeOne, expandOne, prj, on, case_, overOne, mapInductive) where

-- Same API as https://pursuit.purescript.org/packages/purescript-variant

import Prelude

import Control.Alternative (class Alternative, empty)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import FixFunctor (class HFunctor, hmap)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data ValueH :: (Type -> Type) -> Type -> Type

newtype VariantH :: Row ((Type -> Type) -> Type -> Type) -> (Type -> Type) -> Type -> Type
newtype VariantH row f a = VariantH
  { symbol :: String
  , index :: Int -- Order of the type in the row
  , value :: ValueH f a
  }

instance (RowToList row rowlist, HFunctorVariantH rowlist row) => HFunctor (VariantH row) where
  hmap f = hmapVariantH @rowlist @row f

class HFunctorVariantH :: RowList ((Type -> Type) -> Type -> Type) -> Row ((Type -> Type) -> Type -> Type) -> Constraint
class HFunctorVariantH rowlist row | rowlist -> row where
  hmapVariantH :: forall f g. (f ~> g) -> VariantH row f ~> VariantH row g

instance HFunctorVariantH RL.Nil () where
  hmapVariantH _ v = case_ v

instance (HFunctorVariantH rowlist row, HFunctor h, Cons sym h row row2, IsSymbol sym) => HFunctorVariantH (RL.Cons sym h rowlist) row2 where
  hmapVariantH f = mapInductive @sym (hmap f) (hmapVariantH @rowlist f)

toValueH :: forall h f a. h f a -> ValueH f a
toValueH = unsafeCoerce

unsafeFromValueH :: forall h f a. ValueH f a -> h f a
unsafeFromValueH = unsafeCoerce

inj :: forall @sym f h a r1 r2. Cons sym h r1 r2 => IsSymbol sym => h f a -> VariantH r2 f a
inj hfa = VariantH { symbol: reflectSymbol @sym Proxy, index: 0, value: toValueH hfa }

takeOne :: forall @sym f h a r1 r2. Cons sym h r1 r2 => IsSymbol sym => VariantH r2 f a -> Either (VariantH r1 f a) (h f a)
takeOne (VariantH { symbol, index, value }) =
  let
    isSymbolEq = symbol == reflectSymbol @sym Proxy
  in
    if isSymbolEq && index == 0 then Right $ unsafeFromValueH value else Left $ VariantH { symbol, index: if isSymbolEq then index - 1 else 0, value }

expandOne :: forall @sym f h a r1 r2. Cons sym h r1 r2 => IsSymbol sym => VariantH r1 f a -> VariantH r2 f a
expandOne (VariantH { symbol, index, value }) =
  let
    isSymbolEq = symbol == reflectSymbol @sym Proxy
  in
    VariantH { symbol, index: if isSymbolEq then index + 1 else 0, value }

prj :: forall @sym f h a r1 r2 g. Cons sym h r1 r2 => Alternative g => IsSymbol sym => VariantH r2 f a -> g (h f a)
prj v = case takeOne @sym v of
  Left _ -> empty
  Right hfa -> pure hfa

on :: forall @sym h f a b r1 r2. Cons sym h r1 r2 => IsSymbol sym => (h f a -> b) -> (VariantH r1 f a -> b) -> VariantH r2 f a -> b
on f g v = case takeOne @sym v of
  Left v1 -> g v1
  Right hfa -> f hfa

case_ :: forall f a b. VariantH () f a -> b
case_ (VariantH v) = unsafeCrashWith $ "Data.Functor.VariantH: pattern match failure with symbol \"" <> v.symbol <> "\". This may be a library bug."

overOne
  :: forall @sym h1 h2 f g a b r1 r2 r3 r4
   . Cons sym h1 r1 r2
  => Cons sym h2 r4 r3
  => IsSymbol sym
  => (h1 f a -> h2 g b)
  -> (VariantH r1 f a -> VariantH r3 g b)
  -> VariantH r2 f a
  -> VariantH r3 g b
overOne f g v = on @sym h g v
  where
  h = inj @sym <<< f

mapInductive
  :: forall @sym h1 h2 f g a b r1 r2 r3 r4
   . Cons sym h1 r1 r2
  => Cons sym h2 r4 r3
  => IsSymbol sym
  => (h1 f a -> h2 g b)
  -> (VariantH r1 f a -> VariantH r4 g b)
  -> VariantH r2 f a
  -> VariantH r3 g b
mapInductive f g v = overOne @sym f (\v' -> expandOne @sym $ g v') v
