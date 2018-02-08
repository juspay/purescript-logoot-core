module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error, log)
import Data.Container (class Container)
import Logoot.Id (Base(..), prefix, intervalLength)
import Logoot.Types (IdentifierF(..), Position(..))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  testPrefixes
  testIntervalLengths

p :: IdentifierF Array Int Int
p = IdentifierF [Position 2 4 7, Position 59 9 5]
q :: IdentifierF Array Int Int
q = IdentifierF [Position 10 5 3, Position 20 3 6, Position 3 3 9]
b :: Base
b = Base 100
pref :: forall f s c. Container f => Int -> IdentifierF f s c -> Number
pref = prefix b
length :: forall f s c. Container f => Int -> IdentifierF f s c -> IdentifierF f s c -> Int
length = intervalLength b

testPrefixes :: forall e. Eff (console :: CONSOLE | e) Unit
testPrefixes = do
  log "Base: 100"
  log "prefix(p, 1) == 2"
  pref 1 p ==? 2.0
  log "prefix(q, 1) == 10"
  pref 1 q ==? 10.0
  log "prefix(p, 2) == 2.59"
  pref 2 p ==? 2.59
  log "prefix(q, 2) == 10.20"
  pref 2 q ==? 10.20
  log "prefix(p, 3) == 2.59.00"
  pref 3 p ==? 2.5900
  log "prefix(q, 3) == 10.20.03"
  pref 3 q ==? 10.2003

testIntervalLengths :: forall e. Eff (console :: CONSOLE | e) Unit
testIntervalLengths = do
  log "intervalLength(p,q,1) == 7"
  length 1 p q ==? 7
  log "intervalLength(p,q,2) == 760"
  length 2 p q ==? 760
  log "intervalLength(p,q,3) == 76102"
  length 3 p q ==? 76102

shouldEq :: forall a e. Show a => Eq a => a -> a -> Eff (console :: CONSOLE | e) Unit
shouldEq a b
  | a == b = log "Test passed"
  | otherwise = error $ "Failed on " <> show a <> " /= " <> show b

infix 4 shouldEq as ==?
