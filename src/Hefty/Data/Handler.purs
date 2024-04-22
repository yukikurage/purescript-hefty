module Hefty.Data.Handler where

import Prelude

import Control.Monad.Identity.Trans (IdentityT)
import Data.Either (Either(..))
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Hefty.Class.AlgFunctor (class AlgFunctor, algebraicCoerce)
import Hefty.Data.HFunctor (class HFunctor, hmap)
import Hefty.Data.HFunctor.Variant (VariantH, fromSingleton, split)
import Hefty.Data.Hefty (Hefty, mkHefty, resumeHefty')
import Hefty.Data.Row.Meta (class RowMeta)
import Prim.Row (class Cons, class Union)

-- | Both of Handler/Elaboration
-- | `HandlerM` handles all of effects in `r1``, then converts them to effects in `r2`, with the result type `m`
newtype HandlerM r1 r2 m = HandlerM (forall b a hm. (forall x. hm x -> Hefty r2 (m x)) -> VariantH r1 hm b -> (b -> Hefty r2 (m a)) -> Hefty r2 (m a))

handlerM :: forall @sym h r1 r2 m. Cons sym h () r1 => (forall b a hm. (forall x. hm x -> Hefty r2 (m x)) -> h hm b -> (b -> Hefty r2 (m a)) -> Hefty r2 (m a)) -> HandlerM r1 r2 m
handlerM f = HandlerM \hmToHefM vHmB bToHefMa -> f hmToHefM (fromSingleton @sym vHmB) bToHefMa

type Handler r1 r2 = HandlerM r1 r2 Identity

handler :: forall @sym h r1 r2. Cons sym h () r1 => HFunctor h => (forall b a. h (Hefty r2) b -> (b -> Hefty r2 a) -> Hefty r2 a) -> Handler r1 r2
handler f = handlerM @sym \hmToHefM hHmB bToHefMa -> map Identity $ f (hmap (hmToHefM >>> map unwrap) hHmB) (bToHefMa >>> map unwrap)

handleM :: forall r1 r2 a m. Applicative m => HFunctor (VariantH r1) => HandlerM r1 r2 m -> Hefty r1 a -> Hefty r2 (m a)
handleM elb@(HandlerM f) = resumeHefty' alg gen
  where
  alg :: forall b. VariantH r1 (Hefty r1) b -> (b -> Hefty r1 a) -> Hefty r2 (m a)
  alg h cont = f unwrap (hmap (handleM elb >>> Compose) h) (handleM elb <<< cont)

  gen :: a -> Hefty r2 (m a)
  gen = pure >>> pure

handle :: forall r1 r2 a. HFunctor (VariantH r1) => Handler r1 r2 -> Hefty r1 a -> Hefty r2 a
handle hdl hef = unwrap <$> handleM hdl hef

-- | Pass through all of effects.
nil :: forall r. HFunctor (VariantH r) => Handler r r
nil = HandlerM \hmToHefM hHmB bToHefMa -> mkHefty (hmap (hmToHefM >>> map unwrap) hHmB) bToHefMa

-- | Pass through all of effects.
-- | This function requires all of effects in `r` is `AlgFunctor`, because result type `m` disturbs handling internal effects.
-- | If you want to handle with higher-order effects, you should use `nil` (`m` ~ `Identity`) instead.
nilM :: forall r m. AlgFunctor (VariantH r) => HandlerM r r m
nilM = HandlerM \_ hHmB bToHefMa -> mkHefty (algebraicCoerce hHmB) bToHefMa

composeHandler :: forall r1 r2 r3 r m. Union r1 r2 r3 => RowMeta r1 => HandlerM r1 r m -> HandlerM r2 r m -> HandlerM r3 r m
composeHandler (HandlerM f1) (HandlerM f2) = HandlerM \hmToHefM v3HmB bToHefMa -> case split @r1 @r2 v3HmB of
  Left v1HmB -> f1 hmToHefM v1HmB bToHefMa
  Right v2HmB -> f2 hmToHefM v2HmB bToHefMa

infixr 6 composeHandler as &: