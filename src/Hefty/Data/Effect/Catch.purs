module Hefty.Data.Effect.Reader where

import Prelude

import Data.Either (Either(..))
import Hefty.Class.AlgFunctor (class AlgFunctor)
import Hefty.Data.Algebraic (Algebraic(..), handlerAlgM)
import Hefty.Data.Effect.State (THROW)
import Hefty.Data.HFunctor.Variant (VariantH)
import Hefty.Data.Handler (HandlerM, handlerM)
import Hefty.Data.Hefty (Hefty, lift)
import Type.Row (type (+))

data Catch e f a = Catch (f a) (e -> f a)

type Catch_ :: Symbol
type Catch_ = "catch"

type CATCH e r = (catch :: Catch e | r)

catch :: forall e r a. Hefty (CATCH e + r) a -> (e -> Hefty (CATCH e + r) a) -> Hefty (CATCH e + r) a
catch m h = lift @Catch_ $ Catch m h

-- catchHandlerToThrow :: forall e r. HandlerM (CATCH e + ()) (THROW e + r) m
-- catchHandlerToThrow = handlerM @Catch_ \unwrapInternal op _ -> case op of
--   Catch m h -> do
--     ma <- unwrapInternal m