module Hefty.Data.Effect.Local where

import Prelude

import Hefty.Data.Effect.Reader (READER, readerHandler)
import Hefty.Data.HFunctor (class HFunctor)
import Hefty.Data.HFunctor.Variant (class HFunctorV)
import Hefty.Data.Handler (Handler, brace, mkHandler')
import Hefty.Data.Hefty (Hefty, liftOne)
import Type.Row (type (+))

data Local :: forall k. Type -> (k -> Type) -> k -> Type
data Local e f a = Local e (f a)

instance HFunctor (Local e) where
  hmap f (Local e fa) = Local e $ f fa

type Local_ :: Symbol
type Local_ = "local"

type LOCAL :: forall k. Type -> Row ((k -> Type) -> k -> Type) -> Row ((k -> Type) -> k -> Type)
type LOCAL e r = (local :: Local e | r)

local :: forall e r a. e -> Hefty (LOCAL e + r) a -> Hefty (LOCAL e + r) a
local e f = liftOne @Local_ $ Local e f

localHandlerPassThrough :: forall r e. Handler (LOCAL e + ()) r
localHandlerPassThrough = mkHandler' @Local_ \(Local _ internal) -> internal

localHandlerReplaceReader :: forall r e. HFunctorV r => Handler (LOCAL e + ()) (READER e + r)
localHandlerReplaceReader = mkHandler' @Local_ \(Local e internal) -> brace (readerHandler e) internal
