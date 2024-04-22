module Test.Main where

import Prelude

import Data.Either (Either, blush, hush, note)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Hefty.Data.HFunctor.Variant (VariantH, expand, expandOne, expandRight, inj, prj, split, splitOne)

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
  variantH

newtype TestH f a = TestH (f a)

derive instance Newtype (TestH f a) _

variantH :: Effect Unit
variantH = do
  let
    hasSameLabel :: VariantH (x :: TestH, y :: TestH, x :: TestH) Maybe Int
    hasSameLabel = expandOne @"x" $ inj @"x" $ TestH $ Just 1 -- Inject a value into the third slot

  logShow $ prj @"x" hasSameLabel >>= unwrap -- Should print Nothing
  logShow $ (blush $ splitOne @"x" hasSameLabel) >>= prj @"x" >>= unwrap -- Should print Just 1
  logShow $ prj @"y" hasSameLabel >>= unwrap -- Should print Nothing

  let
    expanded :: VariantH (z :: TestH, x :: TestH, y :: TestH, x :: TestH, w :: TestH) Maybe Int
    expanded = expand @(z :: TestH) @_ @(w :: TestH) hasSameLabel

    splited :: Either (VariantH (z :: TestH, x :: TestH, y :: TestH) Maybe Int) (VariantH (x :: TestH, w :: TestH) Maybe Int)
    splited = split expanded

  logShow $ blush splited >>= prj @"x" >>= unwrap -- Should print Nothing
  logShow $ hush splited >>= prj @"x" >>= unwrap -- Should print Just 1

  pure unit