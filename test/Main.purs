module Test.Main where

import Testlude

import Control.Monad.Eff.Ref (Ref, newRef)
import Data.Array as A
import Data.Maybe (Maybe(..))

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
  computedArrayOfLength7 <- getArray 7 clock
  computedArrayOfLength70 <- getArray 70 clock

  printStr "Computed identifier is what we expect:"
  computedArrayOfLength7 ?== artisanallyHandcraftedArrayOfLength7

  printStr "Identifier with 70 elements has expected properties:"
  A.length computedArrayOfLength70 ?== 70
  A.sort computedArrayOfLength70 ?== computedArrayOfLength70
  compare (Just p) (A.head computedArrayOfLength70) ?== LT
  compare (Just q) (A.last computedArrayOfLength70) ?== GT
  where
    getArray :: Int -> Ref Int -> Test e (Array TestId)
    getArray i clock = logootRand b p q i (Boundary 10) (S {id: 0, clock})
    artisanallyHandcraftedArrayOfLength7 :: Array (IdentifierF Array Int Int)
    artisanallyHandcraftedArrayOfLength7 = createIdentifier <$> A.range 1 7
    createIdentifier :: Int -> IdentifierF Array Int Int
    createIdentifier n = IdentifierF [Position (n + 2) (Just 0) (Just n)]