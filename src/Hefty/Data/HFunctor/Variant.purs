module Hefty.Data.HFunctor.Variant (VariantH, class HFunctorVariantH, class HFunctorV, hmapV, hmapVariantH, inj, splitOne, split, expandOne, expandLeft, expandRight, expand, prj, fromSingleton, on, case_, overOne, mapInductive) where

-- Same API as https://pursuit.purescript.org/packages/purescript-variant

import Prelude

import Control.Alternative (class Alternative, empty)
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.List as L
import Data.Symbol (class IsSymbol, reflectSymbol)
import Hefty.Data.HFunctor (class HFunctor, hmap)
import Hefty.Data.Row.Meta (class RowMeta, rowMeta)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons, class Union)
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

class HFunctor (VariantH row) <= HFunctorV row where
  hmapV :: forall f g. (f ~> g) -> VariantH row f ~> VariantH row g

instance HFunctor (VariantH row) => HFunctorV row where
  hmapV = hmap

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

splitOne :: forall @sym f h a r1 r2. Cons sym h r1 r2 => IsSymbol sym => VariantH r2 f a -> Either (VariantH r1 f a) (h f a)
splitOne (VariantH { symbol, index, value }) =
  let
    isSymbolEq = symbol == reflectSymbol @sym Proxy
  in
    if isSymbolEq && index == 0 then Right $ unsafeFromValueH value else Left $ VariantH { symbol, index: if isSymbolEq then index - 1 else 0, value }

split :: forall @r1 @r2 r3 f a. Union r1 r2 r3 => RowMeta r1 => VariantH r3 f a -> Either (VariantH r1 f a) (VariantH r2 f a)
split (VariantH { symbol, index, value }) =
  (if isOnLeft then Left $ VariantH { symbol, index, value } else Right $ VariantH { symbol, index: index - sameLeftSymbolNum, value })
  where
  meta1 = rowMeta @r1
  isOnLeft = elem { symbol: symbol, index: index } meta1.symbols
  sameLeftSymbolNum = L.length $ L.filter (\s -> s.symbol == symbol) meta1.symbols

expandOne :: forall @sym f h a r1 r2. Cons sym h r1 r2 => IsSymbol sym => VariantH r1 f a -> VariantH r2 f a
expandOne (VariantH { symbol, index, value }) =
  let
    isSymbolEq = symbol == reflectSymbol @sym Proxy
  in
    VariantH { symbol, index: if isSymbolEq then index + 1 else 0, value }

expandRight :: forall @r1 @r2 @r3 f a. Union r1 r2 r3 => VariantH r1 f a -> VariantH r3 f a
expandRight = unsafeCoerce

expandLeft :: forall @r1 @r2 @r3 f a. Union r1 r2 r3 => RowMeta r1 => RowMeta r1 => VariantH r2 f a -> VariantH r3 f a
expandLeft (VariantH { symbol, index, value }) = VariantH { symbol, index: index + sameLeftSymbolNum, value }
  where
  meta1 = rowMeta @r1
  sameLeftSymbolNum = L.length $ L.filter (\s -> s.symbol == symbol) meta1.symbols

expand :: forall @r1 @r2 @r3 r4 r5 f a. Union r2 r3 r5 => Union r1 r5 r4 => RowMeta r1 => VariantH r2 f a -> VariantH r4 f a
expand = expandRight @r2 @r3 @r5 >>> expandLeft @r1 @r5 @r4

prj :: forall @sym f h a r1 r2 g. Cons sym h r1 r2 => Alternative g => IsSymbol sym => VariantH r2 f a -> g (h f a)
prj v = case splitOne @sym v of
  Left _ -> empty
  Right hfa -> pure hfa

fromSingleton :: forall @sym h r f a. Cons sym h () r => VariantH r f a -> h f a
fromSingleton (VariantH { value }) = unsafeFromValueH value

on :: forall @sym h f a b r1 r2. Cons sym h r1 r2 => IsSymbol sym => (h f a -> b) -> (VariantH r1 f a -> b) -> VariantH r2 f a -> b
on f g v = case splitOne @sym v of
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
