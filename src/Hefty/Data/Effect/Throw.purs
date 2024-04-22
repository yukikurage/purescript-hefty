module Hefty.Data.Effect.State where

import Prelude

import Data.Either (Either(..))
import Hefty.Data.Algebraic (Algebraic(..), handlerAlgM)
import Hefty.Data.Handler (HandlerM)
import Hefty.Data.Hefty (Hefty, lift)
import Type.Row (type (+))

data Throw :: forall k. Type -> k -> Type
data Throw e a = Throw e

type Throw_ :: Symbol
type Throw_ = "throw"

type THROW :: forall k1 k2. Type -> Row (k1 -> k2 -> Type) -> Row (k1 -> k2 -> Type)
type THROW e r = (throw :: Algebraic (Throw e) | r)

throw :: forall e r a. e -> Hefty (THROW e + r) a
throw e = lift @Throw_ $ Algebraic $ Throw e

throwHandler :: forall e r. HandlerM (THROW e + ()) r (Either e)
throwHandler = handlerAlgM @Throw_ \op _ -> case op of Throw e -> pure $ Left e
