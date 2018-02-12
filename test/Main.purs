module Test.Main where

import Testlude

import Data.Array as A
import Control.Monad.Eff.Ref (Ref, newRef)

main :: forall e. TestEff e Unit
main = do
  log "\nTesting prefixes\n"
  testPrefixes
  log "\nTesting interval lengths\n"
  testIntervalLengths
  log "\nTesting id creation\n"
  testIdCreation

testPrefixes :: forall e. TestEff e Unit
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

testIntervalLengths :: forall e. TestEff e Unit
testIntervalLengths = do
  log "intervalLength(p,q,1) == 7"
  len 1 p q ==? 7
  log "intervalLength(p,q,2) == 760"
  len 2 p q ==? 760
  log "intervalLength(p,q,3) == 76102"
  len 3 p q ==? 76102

testIdCreation :: forall e. TestEff e Unit
testIdCreation = un Test do
  clock <- Test (newRef 0)
  xs <- getArray clock
  -- print (xs == A.sort xs)
  print $ A.filter f <<< un IdentifierF <$> xs
  where
    getArray :: Ref Int -> Test e (Array TestId)
    getArray clock = logootRand (Base 100) p q 6 (Boundary 10) (S {id: 0, clock})
    f :: Position Int Int -> Boolean
    f (Position _ 0 _) = true
    f _ = false
