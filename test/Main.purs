module Test.Main where

import Prelude

import Data.Either (Either, blush, hush, note)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log, logShow)
import Hefty.Data.Effect.Aff (liftAff, runBaseAff)
import Hefty.Data.Effect.Catch (catch, catchHandlerIgnore, catchHandlerToThrow)
import Hefty.Data.Effect.Local (local, localHandlerPassThrough, localHandlerReplaceReader)
import Hefty.Data.Effect.Reader (ask, readerHandler)
import Hefty.Data.Effect.Throw (throw, throwHandlerToEither)
import Hefty.Data.HFunctor.Variant (VariantH, expand, expandOne, expandRight, inj, prj, split, splitOne)
import Hefty.Data.Handler (handle, handleM, (&:))
import Hefty.Data.Hefty (Hefty)

main :: Effect Unit
main = launchAff_ do
  variantH
  hefty

newtype TestH f a = TestH (f a)

derive instance Newtype (TestH f a) _

variantH :: Aff Unit
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

hefty :: Aff Unit
hefty = do
  runBaseAff do
    handle (readerHandler "Global Env") do
      let
        localProgram = local "Local Env" do
          asked <- ask
          pure asked
      maybeGlobal <- handle localHandlerPassThrough localProgram
      maybeLocal <- handle localHandlerReplaceReader localProgram

      liftAff $ logShow maybeGlobal -- "Global Env"
      liftAff $ logShow maybeLocal -- "Local Env"

    let
      catchProgram = do
        catch
          do throw "Error"
          do \_ -> pure "Caught error"
      maybeThrow = handle catchHandlerIgnore catchProgram
      maybeSuccess = handle catchHandlerToThrow catchProgram

    maybeLeft <- handleM throwHandlerToEither maybeThrow
    maybeRight <- handleM throwHandlerToEither maybeSuccess

    liftAff $ logShow maybeLeft -- (Left "Error")
    liftAff $ logShow maybeRight -- (Right "Caught error")
