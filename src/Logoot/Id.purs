module Logoot.Id where

import Prelude

import Control.Plus (empty, (<|>))
import Data.Array as A
import Data.Container (class Container, cons, length, reverse, snoc, take, (!!))
import Data.Foldable as F
import Data.Function (on)
import Data.Int as Z
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Ord (abs)
import Logoot.Types (IdentifierF(IdentifierF), Position(Position), digit, ithClock, ithDigitDefault, ithPeerId)
import Logoot.Types.Class.Site (class Site, siteClock, siteId)
import Math (pow, round)
-- import Debug.Trace (traceShowA)

type IdGenerator m g f i s c
  = IdentifierF f i c -> IdentifierF f i c -> Int -> Boundary -> s -> m (g (IdentifierF f i c))

class Monad m <= MonadLogoot m where
  rand :: Int -> Int -> m Int

-- NOTE: Assumes p < q, so make sure this has been sorted before calling
logootRand
  :: forall g f m s i c
   . MonadLogoot m
  => Container f
  => Container g
  => Site s m i c
  => Base
  -> IdGenerator m g f i s c
logootRand b p q n boundary s = effList where

  indint :: {interval :: Int, index :: Int}
  indint = go {interval: 0, index: 0} where
    go :: {interval :: Int, index :: Int} -> {interval :: Int, index :: Int}
    go acc
      | acc.interval < n = go {index: acc.index + 1, interval: intervalLength b acc.index p q}
      | otherwise = acc

  step :: Int
  step = min (indint.interval / n) (un Boundary boundary)

  r :: f Int
  r = prefixAsContainer b indint.index p

  effList :: m (g (IdentifierF f i c))
  effList = go 1 r empty where
    go :: Int -> f Int -> g (IdentifierF f i c) -> m (g (IdentifierF f i c))
    go j r' lst
      | j <= n = do
        rando <- rand 1 (un Boundary boundary)
        newId <- constructId (r' <|> pure rando) p q s
        go (j+1) (increase r' step) (cons newId lst)
      | otherwise = pure (reverse lst)

  -- NOTES:
  -- This differs from the implementation in the Logoot paper.
  -- It seems as if the paper starts indexing at 1 instead of 0.
  -- Furthermore the lack of typing in the original paper means
  -- that some numbers are treated at different times as
  -- integers, real numbers, and arrays of integers.
  constructId :: f Int -> IdentifierF f i c -> IdentifierF f i c -> s -> m (IdentifierF f i c)
  constructId r' p' q' s' = go 0 (IdentifierF empty) where
    go :: Int -> IdentifierF f i c -> m (IdentifierF f i c)
    go i idf
      | i < length r' -- TODO: Check if this should be < or <=
      , Just d <- r' !! i
      , Just s'' <- p `ithPeerId` i
      , Just c'' <- p `ithClock` i
      , d == ithDigitDefault p i = go (i+1) (consId (Position d s'' c'') idf)
      | i <= length r'
      , Just d <- r' !! i
      , Just s'' <- q `ithPeerId` i
      , Just c'' <- q `ithClock` i
      , d == ithDigitDefault q i = go (i+1) (consId (Position d s'' c'') idf)
      | i <= length r'
      , Just d <- r' !! i = do
        s'' <- siteId s'
        c'' <- siteClock s'
        go (i+1) (consId (Position d s'' c'') idf)
      | otherwise = pure idf

  -- NOTE: If we use `cons` instead of `snoc` we may be mixing up endianness
  consId :: Position i c -> IdentifierF f i c -> IdentifierF f i c
  consId pos (IdentifierF xs) = IdentifierF (snoc xs pos)

  -- TODO: This can probably be optimized. As it's written now, it's treating a
  -- value of type `f Int` as a number, and the idea is to "increase" the first
  -- argument `xs` by the second argument `i`. Here, "increase" means a function
  -- such that the resulting value `ys :: f Int` will have the property that
  -- `metric b ys - metric b xs ≈ i`.` We do this by effectively performing
  -- a binary search over the space of `f Int` to find such a value.
  increase :: f Int -> Int -> f Int
  increase xs i = xs

-- type IdGenerator m g f i s c
--   = IdentifierF f i c -> IdentifierF f i c -> Int -> Boundary -> s -> m (g (IdentifierF f i c))
-- midpt :: forall m g f i s c. Monad m => Container g => Container f => Site s i c => Clock c => Base -> IdGenerator m g f
-- midpt b p q n boundary site = mList where
--   m
--   ids = prefixAsArray b n p

-- Treats a value `ds :: f Int` in base `b` as a "real number" where each int is
-- a digit. This function returns the "magnitude" of `ds`, or the "distance" from 0.
metric :: forall f. F.Foldable f => Base -> f Int -> Number
metric (Base b) ds = (tidy (F.length ds) <<< _.val <<< A.foldl f {ind: 0, val: 0.0}) ds where
  f :: {ind :: Int, val :: Number} -> Int -> {ind :: Int, val :: Number}
  f {ind, val} n = {ind: ind - 1, val: val + Z.toNumber n*b^ind}
  tidy :: Int -> Number -> Number -- gets rid of pesky floating point errors
  tidy l x = round (x*b^l) / b^l

prefixAsContainer
  :: forall f i c. Container f
  => Base -> Int -> IdentifierF f i c -> f Int
prefixAsContainer base n = map digit <<< take n <<< un IdentifierF

prefix
  :: forall f i c. Container f
  => Base -> Int -> IdentifierF f i c -> Number
prefix base n = metric base <<< prefixAsContainer base n

intervalLength
  :: forall f i c. Container f
  => Base -> Int -> IdentifierF f i c -> IdentifierF f i c -> Int
intervalLength b@(Base b') n p q =
  let
    p' = prefix b n p
    q' = prefix b n q
  in
    Z.round (abs (p'*b'^(n-1) - q'*b'^(n-1)) - 1.0)

-- Utilities

raise :: Int -> Int -> Number
raise = pow `on` Z.toNumber

infixr 8 raise as ^

newtype Base = Base Int

derive instance newtypeBase :: Newtype Base _

newtype Boundary = Boundary Int

derive instance newtypeBoundary :: Newtype Boundary _