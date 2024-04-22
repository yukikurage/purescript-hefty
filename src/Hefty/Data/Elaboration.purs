module Hefty.Data.Elaboration where

import Prelude

import Data.HFunctor.Variant (VariantH)
import FixFunctor (class HFunctor, hmap)
import Hefty.Data.Hefty (Hefty, mkHefty, resumeHefty')

newtype Elaboration r1 r2 = Elaboration (forall b a. VariantH r1 (Hefty r2) b -> (b -> Hefty r2 a) -> Hefty r2 a)

elaborate :: forall r1 r2 a. HFunctor (VariantH r1) => Elaboration r1 r2 -> Hefty r1 a -> Hefty r2 a
elaborate elb@(Elaboration f) = resumeHefty' alg gen
  where
  alg :: forall b. VariantH r1 (Hefty r1) b -> (b -> Hefty r1 a) -> Hefty r2 a
  alg h cont = f (hmap (elaborate elb) h) (elaborate elb <<< cont)

  gen :: a -> Hefty r2 a
  gen = pure

passThrow :: forall r. Elaboration r r
passThrow = Elaboration mkHefty

-- composeElaboration :: forall @sym h r1 r2 m. IsSymbol sym => Cons sym h r1 r2 => Elaboration h m -> Elaboration r1 m -> Elaboration r2 m
-- composeElaboration (Elaboration f1) (Elaboration f2) = Elaboration \v2b bToMa -> case takeOne @sym v2b of
--   Left v1b -> f2 v1b bToMa
--   Right h -> f1 h bToMa
