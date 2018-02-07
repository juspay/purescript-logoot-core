module Logoot.Id where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array as A
import Data.Foldable (class Foldable, length)
import Data.Function (on)
import Data.Int as Z
import Data.Newtype (class Newtype, un)
import Data.Ord (abs)
import Logoot.Types (IdentifierF(IdentifierF), digit)
import Math (pow, round)

type IdGenerator m g f s c
  = IdentifierF f s c -> IdentifierF f s c -> Int -> Number -> s -> m (g (IdentifierF f s c))

logootRand :: forall e s c. IdGenerator (Eff (random :: RANDOM | e)) Array Array s c
logootRand p q n boundary s = pure []

metric :: forall f. Foldable f => Base -> f Int -> Number
metric (Base b) ds = (tidy (length ds) <<< _.val <<< A.foldl f {ind: 0, val: 0.0}) ds where
  f :: {ind :: Int, val :: Number} -> Int -> {ind :: Int, val :: Number}
  f {ind, val} n = {ind: ind - 1, val: val + Z.toNumber n*b^ind}
  tidy :: Int -> Number -> Number -- gets rid of pesky floating point errors
  tidy l x = round (x*b^l) / b^l

prefix
  :: forall f s c . Foldable f => Functor f
  => (forall a. Int -> f a -> f a)
  -> Base -> Int -> IdentifierF f s c -> Number
prefix take base n = metric base <<< map digit <<< take n <<< un IdentifierF

intervalLength
  :: forall f s c. Foldable f => Functor f => (forall a. Int -> f a -> f a)
  -> Base -> Int -> IdentifierF f s c -> IdentifierF f s c -> Int
intervalLength take b@(Base b') n p q =
  let
    p' = prefix take b n p
    q' = prefix take b n q
  in
    Z.round (abs (p'*b'^(n-1) - q'*b'^(n-1)) - 1.0)

-- Utilities

raise :: Int -> Int -> Number
raise = pow `on` Z.toNumber

infixr 8 raise as ^

newtype Base = Base Int

derive instance newtypeBase :: Newtype Base _