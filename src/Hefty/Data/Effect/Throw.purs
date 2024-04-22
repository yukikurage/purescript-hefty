module Hefty.Data.Effect.Throw where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Effect.Exception (error)
import Hefty.Data.Algebraic (Algebraic(..), mkHandlerAlgM)
import Hefty.Data.Effect.Aff (AFF, liftAff)
import Hefty.Data.Handler (HandlerM)
import Hefty.Data.Hefty (Hefty, liftOne)
import Type.Row (type (+))

data Throw :: forall k. Type -> k -> Type
data Throw e a = Throw e

type Throw_ :: Symbol
type Throw_ = "throw"

type THROW :: forall k1 k2. Type -> Row (k1 -> k2 -> Type) -> Row (k1 -> k2 -> Type)
type THROW e r = (throw :: Algebraic (Throw e) | r)

throw :: forall e r a. e -> Hefty (THROW e + r) a
throw e = liftOne @Throw_ $ Algebraic $ Throw e

throwHandlerToEither :: forall @r e. HandlerM (THROW e + ()) r (Either e)
throwHandlerToEither = mkHandlerAlgM @Throw_ \(Throw e) _ -> pure $ Left e

throwHandlerToAff :: forall @r e m. (e -> String) -> HandlerM (THROW e + ()) (AFF + r) m
throwHandlerToAff f = mkHandlerAlgM @Throw_ \(Throw e) _ -> liftAff $ throwError $ error $ f e
