module Logoot.Id where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array as A
import Data.Foldable (class Foldable, length)
import Data.Function (on)
import Data.Int (Radix, toNumber)
import Data.Newtype (class Newtype, un)
import Logoot.Types (IdentifierF(..), Position(..), digit)
import Math (pow, round)

type IdGenerator m g f s c
  = IdentifierF f s c -> IdentifierF f s c -> Int -> Number -> s -> m (g (IdentifierF f s c))

-- logootRand :: forall e s c. IdGenerator (Eff (random :: RANDOM | e)) Array Array s c
-- logootRand p q n boundary s = pure []

prefix
  :: forall f s c . Foldable f => Functor f
  => (forall a. Int -> f a -> f a) -> Base -> Int -> IdentifierF f s c -> Number
prefix take base n = metric base <<< map digit <<< take n <<< un IdentifierF

metric :: forall f. Foldable f => Base -> f Int -> Number
metric (Base b) ds = (tidy (length ds) <<< _.val <<< A.foldl f {ind: 0, val: 0.0}) ds where
  f :: {ind :: Int, val :: Number} -> Int -> {ind :: Int, val :: Number}
  f {ind, val} n = {ind: ind - 1, val: val + toNumber n*b^ind}
  tidy :: Int -> Number -> Number -- gets rid of pesky floating point errors
  tidy l x = round (x*b^l) / b^l

-- Need to turn identifiers into a metric: a function f :: Identifier -> Number
-- that satisfies the usual metric space properties
-- If f is invertible then we can project onto Numbers, operate on them, and pull
-- back.

-- Can't we treat Array Int as a number by itself?
-- Assuming x :: Array Int maps to a Number f(x) \in [0, 1] where f^-1(0) is the
-- document start and f^-1(1) is the document's end.

-- Utilities

raise :: Int -> Int -> Number
raise = pow `on` toNumber

infixr 8 raise as ^

newtype Base = Base Int

derive instance newtypeBase :: Newtype Base _