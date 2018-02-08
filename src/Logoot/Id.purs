module Logoot.Id where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Plus (empty)
import Data.Array as A
import Data.Container (class Container, take, snoc, length)
import Data.Foldable as F
import Data.Function (on)
import Data.Int as Z
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Ord (abs)
import Data.String as S
import Logoot.Types (IdentifierF(IdentifierF), Position(Position), digit, ithClock, ithDigitDefault, ithPeerId)
import Logoot.Types.Class.Site (class Site, siteClock, siteId)
import Logoot.Types.Class.Clock (class Clock, cpp)
import Math (pow, round)

type IdGenerator m g f i s c
  = IdentifierF f i c -> IdentifierF f i c -> Int -> Boundary -> s -> m (g (IdentifierF f i c))

-- NOTE: Assumes p < q, so make sure this has been sorted before calling
logootRand
  :: forall f e s i c
  . Container f
  => Site s i c
  => Clock c
  => Base
  -> IdGenerator (Eff (random :: RANDOM | e)) L.List f i s c
logootRand b p q n boundary s = effList
  where

  indint :: {interval :: Int, index :: Int}
  indint = go {interval: 0, index: 0} where
    go :: {interval :: Int, index :: Int} -> {interval :: Int, index :: Int}
    go acc
      | acc.interval < n = go {index: acc.index + 1, interval: intervalLength b acc.index p q}
      | otherwise = acc

  step :: Int
  step = min (indint.interval / n) (un Boundary boundary) -- NOTE: Unsure whether this should be int or number

  r :: Number
  r = prefix b indint.index p

  effList :: Eff (random :: RANDOM | e) (L.List (IdentifierF f i c))
  effList = go 1 r L.Nil where
    go :: Int -> Number -> L.List (IdentifierF f i c) -> Eff (random :: RANDOM | e) (L.List (IdentifierF f i c))
    go j r' lst
      | j <= n = do
        rand <- randomInt 1 (un Boundary boundary) -- NOTE: Unsure whether this should be an int or a number
        go (j+1) (r' + Z.toNumber step) (L.Cons (constructId (r' + Z.toNumber rand) p q s) lst)
      | otherwise = pure lst

  -- NOTES:
  -- This differs from the implementation in the Logoot paper.
  -- It seems as if the paper starts indexing at 1 instead of 0.
  -- Furthermore the lack of typing in the original paper means
  -- that some numbers are treated at different times as
  -- integers, real numbers, and arrays of integers.
  constructId :: Number -> IdentifierF f i c -> IdentifierF f i c -> s -> IdentifierF f i c
  constructId r' p' q' s' = go 0 (IdentifierF empty) where
    go :: Int -> IdentifierF f i c -> IdentifierF f i c
    go i idf
      | i <= getLength p' q' -- See note for getLength
      , Just d <- r `getIthDigit` i
      , Just s'' <- p `ithPeerId` i
      , Just c'' <- p `ithClock` i
      , d == ithDigitDefault p i = go (i+1) (consId (Position d s'' c'') idf)
      | i <= getLength p' q'
      , Just d <- r `getIthDigit` i
      , Just s'' <- q `ithPeerId` i
      , Just c'' <- q `ithClock` i
      , d == ithDigitDefault q i = go (i+1) (consId (Position d s'' c'') idf)
      | i <= getLength p' q'
      , s'' <- siteId s'
      , Just d <- r `getIthDigit` i
      , c'' <- siteClock s' = go (i+1) (consId (Position d s'' (cpp c'')) idf)
      | otherwise = idf

  getIthDigit :: Number -> Int -> Maybe Int
  getIthDigit n i = A.catMaybes (map (Z.fromString <<< S.singleton) (S.toCharArray $ show n)) A.!! i

  -- NOTE: Instead of passing in only one argument r, as in the paper, we pass in
  -- both identifiers for the following reasons:
  -- 1. r is defined to have a random component, and if this needs to be a number
  -- instead of an integer, r could be "overspecified": It could have more significant
  -- digits than are strictly necessary, which could slow down performance over time.
  -- 2. Either p or q could have longer length than the other, but we only need
  -- the length of the smallest, plus one (to get a bit of information from the
  -- random component).
  getLength :: IdentifierF f i c -> IdentifierF f i c -> Int
  getLength (IdentifierF xs) (IdentifierF ys) = min (length xs) (length ys)

  -- TODO: We may be mixing up endianness here
  consId :: Position i c -> IdentifierF f i c -> IdentifierF f i c
  consId p (IdentifierF xs) = IdentifierF (snoc xs p)

metric :: forall f. F.Foldable f => Base -> f Int -> Number
metric (Base b) ds = (tidy (F.length ds) <<< _.val <<< A.foldl f {ind: 0, val: 0.0}) ds where
  f :: {ind :: Int, val :: Number} -> Int -> {ind :: Int, val :: Number}
  f {ind, val} n = {ind: ind - 1, val: val + Z.toNumber n*b^ind}
  tidy :: Int -> Number -> Number -- gets rid of pesky floating point errors
  tidy l x = round (x*b^l) / b^l

prefixAsArray
  :: forall f i c. Container f
  => Base -> Int -> IdentifierF f i c -> f Int
prefixAsArray base n = map digit <<< take n <<< un IdentifierF

prefix
  :: forall f i c. Container f
  => Base -> Int -> IdentifierF f i c -> Number
prefix base n = metric base <<< prefixAsArray base n

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