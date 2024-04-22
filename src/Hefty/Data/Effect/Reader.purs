module Hefty.Data.Effect.Reader where

import Prelude

import Hefty.Data.Algebraic (Algebraic(..), mkHandlerAlgM)
import Hefty.Data.Handler (HandlerM)
import Hefty.Data.Hefty (Hefty, liftOne)
import Type.Row (type (+))

data Reader e a = Reader (e -> a)

type Reader_ :: Symbol
type Reader_ = "reader"

type READER :: forall k. Type -> Row (k -> Type -> Type) -> Row (k -> Type -> Type)
type READER e r = (reader :: Algebraic (Reader e) | r)

ask :: forall e r. Hefty (READER e + r) e
ask = liftOne @Reader_ $ Algebraic $ Reader identity

readerHandler :: forall r e m. e -> HandlerM (READER e + ()) r m
readerHandler e = mkHandlerAlgM @Reader_ \(Reader f) cont -> cont $ f e
