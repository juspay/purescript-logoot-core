module Testlude
  ( module Exports
  -- constants
  , p, q, b
  -- utilities
  , pref, len
  , shouldEq, (==?)
  , print, printStr
  , shouldEqTest, (?==)
  -- data types
  , S(..)
  , Test(..)
  -- aliases
  , TestId
  , TestEff
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error, log)
import Control.Monad.Eff.Console (error, log) as Exports
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef')
import Data.Container (class Container)
import Data.Container (class Container, take, drop, cons, index) as Exports
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype (un) as Exports
import Logoot.Id (Base(..), Boundary(..), intervalLength, logootRand, prefix) as Exports
import Logoot.Id (class MonadLogoot, Base(..), intervalLength, prefix)
import Logoot.Types (IdentifierF(..), Position(..))
import Logoot.Types (IdentifierF(..), Position(..)) as Exports
import Logoot.Types.Class.Site (class Site)
import Logoot.Types.Class.Site (class Site) as Exports
import Prelude (class Applicative, class Apply, class Bind, class BooleanAlgebra, class Bounded, class Category, class CommutativeRing, class Discard, class DivisionRing, class Eq, class EuclideanRing, class Field, class Functor, class HeytingAlgebra, class Monad, class Ord, class Ring, class Semigroup, class Semigroupoid, class Semiring, class Show, type (~>), Ordering(EQ, GT, LT), Unit, Void, absurd, add, ap, append, apply, between, bind, bottom, clamp, compare, comparing, compose, conj, const, degree, discard, disj, div, eq, flap, flip, gcd, id, ifM, join, lcm, liftA1, liftM1, map, max, min, mod, mul, negate, not, notEq, one, otherwise, pure, recip, show, sub, top, unit, unless, unlessM, void, when, whenM, zero, (#), ($), ($>), (&&), (*), (*>), (+), (-), (/), (/=), (<), (<#>), (<$), (<$>), (<*), (<*>), (<<<), (<=), (<=<), (<>), (<@>), (=<<), (==), (>), (>=), (>=>), (>>=), (>>>), (||)) as Exports

shouldEq :: forall a e. Show a => Eq a => a -> a -> Eff (console :: CONSOLE | e) Unit
shouldEq x y
  | x == y = log "Test passed"
  | otherwise = error $ "Failed on " <> show x <> " /= " <> show y

infix 4 shouldEq as ==?

p :: IdentifierF Array Int Int
p = IdentifierF [Position 2 (Just 4) (Just 7), Position 59 (Just 9) (Just 5)]

q :: IdentifierF Array Int Int
q = IdentifierF [Position 10 (Just 5) (Just 3), Position 20 (Just 3) (Just 6), Position 3 (Just 3) (Just 9)]

b :: Base
b = Base 100

pref :: forall f s c. Container f => Int -> IdentifierF f s c -> Number
pref = prefix b

len :: forall f s c. Container f => Int -> IdentifierF f s c -> IdentifierF f s c -> Int
len = intervalLength b

newtype S = S {id :: Int, clock :: Ref Int}

instance siteS :: Site S (Test e) Int Int where
  siteId (S {id}) = pure (Just id)
  siteClock (S {clock}) =
    Test $ modifyRef' clock \ s -> {state: s+1, value: Just (s+1)}

type TestId = IdentifierF Array Int Int

type TestEff e a = Eff (ref :: REF, console :: CONSOLE, random :: RANDOM | e) a

newtype Test e a = Test (Eff (ref :: REF, console :: CONSOLE, random :: RANDOM | e) a)

derive instance newtypeTest :: Newtype (Test e a) _
derive newtype instance functorTest :: Functor (Test e)
derive newtype instance applyTest :: Apply (Test e)
derive newtype instance applicativeTest :: Applicative (Test e)
derive newtype instance bindTest :: Bind (Test e)
derive newtype instance monadTest :: Monad (Test e)

instance monadLogootTest :: MonadLogoot (Test e) where
  rand beg end = Test (randomInt beg end)

print :: forall a e. Show a => a -> Test e Unit
print = printStr <<< show

printStr :: forall e. String -> Test e Unit
printStr = Test <<< log

shouldEqTest :: forall a e. Show a => Eq a => a -> a -> Test e Unit
shouldEqTest x y = Test (shouldEq x y)

infix 4 shouldEqTest as ?==