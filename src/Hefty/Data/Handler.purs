module Hefty.Data.Handler where

import Prelude

import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Hefty.Class.AlgFunctor (class AlgFunctorV, algebraicCoerceV)
import Hefty.Data.HFunctor (class HFunctor)
import Hefty.Data.HFunctor.Variant (class HFunctorV, VariantH, fromSingleton, hmapV, split)
import Hefty.Data.Hefty (Hefty, expandLeft, mkHeftyAll, resumeHefty)
import Hefty.Data.Row.Meta (class RowMeta)
import Prim.Row (class Cons, class Union)

-- | Both of Handler/Elaboration
-- | `HandlerM` handles all of effects in `r1`, then converts them to effects in `r2`, with the result type `m`
newtype HandlerM :: forall k. Row ((Type -> Type) -> Type -> Type) -> Row ((Type -> Type) -> Type -> Type) -> (k -> Type) -> Type
newtype HandlerM r1 r2 m = HandlerM
  ( forall b a
     . VariantH r1 (Hefty r2) b -- Operation, second argument (Hefty r2) means the internal effect type, if only (`m` ~ `Identity`, otherwise just phantom type)
    -> (b -> Hefty r2 (m a))
    -> Hefty r2 (m a)
  )

mkHandlerM
  :: forall @sym h r1 r2 m
   . Cons sym h () r1
  => (forall b a f. h f b -> (b -> Hefty r2 (m a)) -> Hefty r2 (m a))
  -> HandlerM r1 r2 m
mkHandlerM f = HandlerM \vHefB bToHefMa -> f (fromSingleton @sym vHefB) bToHefMa

type Handler r1 r2 = HandlerM r1 r2 Identity

mkHandler :: forall @sym h r1 r2. Cons sym h () r1 => HFunctor h => (forall b a. h (Hefty r2) b -> (b -> Hefty r2 a) -> Hefty r2 a) -> Handler r1 r2
mkHandler f = HandlerM \vHmB bToHefMa -> map Identity $ f (fromSingleton @sym vHmB) (bToHefMa >>> map unwrap)

mkHandler' :: forall @sym h r1 r2. Cons sym h () r1 => HFunctor h => (h (Hefty r2) ~> Hefty r2) -> Handler r1 r2
mkHandler' f = mkHandler @sym \hHefB bToHefA -> f hHefB >>= bToHefA

handleM
  :: forall r1 r2 r3 a m
   . Union r1 r2 r3
  => RowMeta r1
  => Applicative m
  => AlgFunctorV r1
  => AlgFunctorV r2
  => HandlerM r1 r2 m
  -> Hefty r3 a
  -> Hefty r2 (m a)
handleM hdl@(HandlerM f) = resumeHefty alg gen
  where
  alg :: forall b. VariantH r3 (Hefty r3) b -> (b -> Hefty r3 a) -> Hefty r2 (m a)
  alg h3 cont = case split @r1 @r2 h3 of
    Left h1 -> f (algebraicCoerceV h1) (cont >>> handleM hdl)
    Right h2 -> mkHeftyAll (algebraicCoerceV h2) $ cont >>> handleM hdl

  gen :: a -> Hefty r2 (m a)
  gen = pure >>> pure

braceM
  :: forall r1 r2 r3 a m
   . Union r1 r2 r3
  => HFunctorV r2
  => AlgFunctorV r2
  => AlgFunctorV r1
  => RowMeta r1
  => Applicative m
  => HandlerM r1 r2 m
  -> Hefty r3 a
  -> Hefty r3 (m a)
braceM hdl hef = expandLeft @r1 @r2 $ (handleM hdl hef :: Hefty r2 (m a))

handle
  :: forall r1 r2 r3 a
   . Union r1 r2 r3
  => RowMeta r1
  => HFunctorV r1
  => HFunctorV r2
  => Handler r1 r2
  -> Hefty r3 a
  -> Hefty r2 a
handle hdl@(HandlerM f) = resumeHefty alg gen
  where
  alg :: forall b. VariantH r3 (Hefty r3) b -> (b -> Hefty r3 a) -> Hefty r2 a
  alg h3 cont = case split @r1 @r2 h3 of
    Left h1 -> map unwrap $ f (hmapV (handle hdl) h1) (cont >>> handle hdl >>> map Identity)
    Right h2 -> mkHeftyAll (hmapV (handle hdl) h2) $ cont >>> handle hdl

  gen :: a -> Hefty r2 a
  gen = pure

brace
  :: forall r1 r2 r3 a
   . Union r1 r2 r3
  => HFunctorV r1
  => HFunctorV r2
  => RowMeta r1
  => Handler r1 r2
  -> Hefty r3 a
  -> Hefty r3 a
brace hdl hef = expandLeft @r1 @r2 $ (handle hdl hef :: Hefty r2 a)

-- | Pass through all of effects.
nil :: forall r. HFunctorV r => Handler r r
nil = HandlerM mkHeftyAll

composeHandler :: forall r1 r2 r3 r m. Union r1 r2 r3 => RowMeta r1 => HandlerM r1 r m -> HandlerM r2 r m -> HandlerM r3 r m
composeHandler (HandlerM f1) (HandlerM f2) = HandlerM \v3HmB bToHefMa -> case split @r1 @r2 v3HmB of
  Left v1HmB -> f1 v1HmB bToHefMa
  Right v2HmB -> f2 v2HmB bToHefMa

infixr 6 composeHandler as &:
