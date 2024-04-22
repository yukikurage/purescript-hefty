module Hefty.Data.Effect.Catch where

import Prelude

import Data.Either (Either(..))
import Hefty.Class.AlgFunctor (class AlgFunctorV)
import Hefty.Data.Effect.Throw (THROW, throwHandlerToEither)
import Hefty.Data.HFunctor (class HFunctor)
import Hefty.Data.HFunctor.Variant (class HFunctorV)
import Hefty.Data.Handler (Handler, braceM, mkHandler')
import Hefty.Data.Hefty (Hefty, liftOne)
import Type.Row (type (+))

data Catch :: forall k. Type -> (k -> Type) -> k -> Type
data Catch e f a = Catch (f a) (e -> f a)

instance HFunctor (Catch e) where
  hmap f (Catch m h) = Catch (f m) (h >>> f)

type Catch_ :: Symbol
type Catch_ = "catch"

type CATCH :: forall k1. Type -> Row ((k1 -> Type) -> k1 -> Type) -> Row ((k1 -> Type) -> k1 -> Type)
type CATCH e r = (catch :: Catch e | r)

catch :: forall e r a. Hefty (CATCH e + r) a -> (e -> Hefty (CATCH e + r) a) -> Hefty (CATCH e + r) a
catch m h = liftOne @Catch_ $ Catch m h

catchHandlerToThrow
  :: forall e r
   . AlgFunctorV r
  => HFunctorV r
  => Handler (CATCH e + ()) (THROW e + r)
catchHandlerToThrow = mkHandler' @Catch_ \(Catch m h) -> do
  eOrA <- braceM throwHandlerToEither m
  case eOrA of
    Left e -> h e
    Right a -> pure a

catchHandlerIgnore :: forall e r. AlgFunctorV r => Handler (CATCH e + ()) r
catchHandlerIgnore = mkHandler' @Catch_ \(Catch m _) -> m
