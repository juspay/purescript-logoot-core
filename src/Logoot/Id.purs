module Logoot.Id where

import Prelude

import Control.Plus (empty)
import Data.Array as A
import Data.Container (class Container, cons, dropWhile, findIndex, reverse, set, take, (!!))
import Data.Foldable as F
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Z
import Data.Maybe (Maybe(Just))
import Data.Newtype (class Newtype, un)
import Data.Ord (abs)
import Data.TraversableWithIndex (traverseWithIndex)
import Logoot.Types (IdentifierF(IdentifierF), Position(Position), digit, ithClock, ithDigit, ithPeerId)
import Logoot.Types.Class.Site (class Site, siteClock, siteId)
import Math (pow, round)
import Partial.Unsafe (unsafeCrashWith)

type IdGenerator m g f i s c
  = Base
  -> IdentifierF f i c
  -> IdentifierF f i c
  -> Int
  -> Boundary
  -> s
  -> m (g (IdentifierF f i c))

class Monad m <= MonadLogoot m where
  rand :: Int -> Int -> m Int

-- NOTE: Assumes p < q, so make sure this has been sorted before calling
-- Given two identifiers p and q, a base b, integer n, boundary and site:
-- First calculate the "depth" of the generated identifiers. This depends
-- on how many we want. For example, if p = <2,4,7><59,9,5> and
-- q = <10,5,3><20,3,6><3,3,9>, then:
-- a) we can find 7 = 10 - 2 - 1 identifiers between p and q of length 1 (the
-- ones in the set (2, 10) = [3, 9])
-- b) we can find 760 = 1020 - 259 - 1 identifiers between p and q of length 2
-- (the ones in the set (2.59, 10.20) = [2.60, 10.19]
-- c) 76102 identifiers of length 3, etc.
-- So, we need to find the length of the generated identifiers which depends on
-- the value n.
-- Once we do that, there are actually n intervals r_i between p and q where we can
-- choose any identifier r_i^* ∈ r_i. We choose these at random (hence the name).
-- These intervals will be the minimum of the given boundary argument, and the
-- calculated interval length (that only depends on p, q and n).
-- Having chosen n identifiers r_i^* from each interval r_i, 1 <= i <= n, we are
-- done.
logootRand
  :: forall g f m s i c
   . MonadLogoot m => Show (f Int)
  => Container f
  => Container g
  => Site s m i c
  => IdGenerator m g f i s c
logootRand b p q n boundary s = effList where

  -- Constructs n identifiers between p and q
  effList :: m (g (IdentifierF f i c))
  effList = go 0 (prefixAsContainer b (intervalInfo.depth + 1) p) empty where
    go :: Int -> f Int -> g (IdentifierF f i c) -> m (g (IdentifierF f i c))
    go i rDigits acc
      | i < n = do
        leastSigFig <- rand 1 intervalInfo.dist
        let
          r = digitallyAdd rDigits leastSigFig
          rDigits' = digitallyAdd rDigits intervalInfo.dist
        gen'dId <- generateId (nubZeroes r)
        go (i + 1) rDigits' (cons gen'dId acc)
      | otherwise = pure (reverse acc)

  nubZeroes :: f Int -> f Int
  nubZeroes = reverse <<< dropWhile (_ == 0) <<< reverse

  -- Figures out how many significant digits the generated identifiers need
  -- (depth), and how far apart the identifers are wrt their least significant digit
  -- (dist)
  intervalInfo :: {depth :: Int, dist :: Int}
  intervalInfo = go 1 where
    go :: Int -> {depth :: Int, dist :: Int}
    go i
      | intervalLength b i p q < n = -- not enough available ids
        go (i + 1) -- so we need more sigfigs
      | otherwise = -- available ids are >= # requested ids
        { depth: i - 1 -- since we're 0-indexed
        , dist: max 1 (min (intervalLength b i p q / n) (un Boundary boundary))
        }
  
  generateId :: f Int -> m (IdentifierF f i c)
  generateId r = IdentifierF <$> traverseWithIndex f r where
    f :: Int -> Int -> m (Position i c)
    f i d
      | Just d' <- p `ithDigit` i
      , Just pid <- p `ithPeerId` i
      , Just clock <- p `ithClock` i
      , d' == d = pure (Position d pid clock)
      | Just d' <- q `ithDigit` i
      , Just pid <- q `ithPeerId` i
      , Just clock <- q `ithClock` i
      , d' == d = pure (Position d pid clock)
      | otherwise = Position d <$> siteId s <*> siteClock s

  -- Adds a least significant digit to a list of digits, handling spillover etc
  -- Assumes ds always has a length of intervalInfo.depth + 1
  digitallyAdd :: f Int -> Int -> f Int
  digitallyAdd ds lsd
    | {depth} <- intervalInfo -- everything is peachy
    , Just d <- ds !! depth
    , base <- un Base b
    , d' <- d + lsd
    , d' < base = set ds depth d'
    | {depth} <- intervalInfo -- shit, we need to increment a more significant digit
    , Just d <- ds !! depth
    , base <- un Base b
    , ds' <- set ds depth (base - 1)
    , d' <- (d + lsd) `mod` base = set (succ ds') depth d'
    | otherwise = unsafeCrashWith "The impossible happened!"
    -- Getting the wrong sigdig, and incorrectly setting it to the base for some reason

  -- Assumes the argument has length intervalInfo.depth + 1
  -- Finds the significance (index) of the digit that must be incremented, increments
  -- it and sets all digits of lower significance to 0
  succ :: f Int -> f Int
  succ ds
    | ds' <- reverse ds
    , Base base <- b
    , Just i <- findIndex (\ x -> x + 1 < base) ds' =
      reverse $ flip mapWithIndex ds' \ idx -> case compare idx i of
        LT -> const 0
        EQ -> (_ + 1)
        GT -> id
    | otherwise = unsafeCrashWith "The impossible happened!" -- this should be impossible

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
intervalLength b@(Base b') i p q =
  let
    p' = prefix b i p
    q' = prefix b i q
  in
    Z.round (abs (p'*b'^(i-1) - q'*b'^(i-1)) - 1.0)

-- Utilities

raise :: Int -> Int -> Number
raise = pow `on` Z.toNumber

infixr 8 raise as ^

newtype Base = Base Int

derive instance newtypeBase :: Newtype Base _

newtype Boundary = Boundary Int

derive instance newtypeBoundary :: Newtype Boundary _