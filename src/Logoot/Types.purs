module Logoot.Types
  ( module Exports
  , digit
  , peerId
  , clock
  , ithDigit
  , ithDigitDefault
  , ithPeerId
  , ithClock
  ) where

import Prelude

import Data.Container (class Container, (!!))
import Data.Maybe (Maybe, fromMaybe)
import Logoot.Types.Identifier (IdentifierF(..))
import Logoot.Types.Identifier (IdentifierF(..)) as Exports
import Logoot.Types.Position (Position(..))
import Logoot.Types.Position (Position(..)) as Exports

digit :: forall peerId clock. Position peerId clock -> Int
digit (Position i _ _) = i

peerId :: forall peerId clock. Position peerId clock -> peerId
peerId (Position _ s _) = s

clock :: forall peerId clock. Position peerId clock -> clock
clock (Position _ _ c) = c

ithElt :: forall f a peerId clock. Container f => (Position peerId clock -> a) -> IdentifierF f peerId clock -> Int -> Maybe a
ithElt f (IdentifierF xs) n = f <$> xs !! n

ithDigit :: forall f peerId clock. Container f => IdentifierF f peerId clock -> Int -> Maybe Int
ithDigit = ithElt digit

ithDigitDefault :: forall f peerId clock. Container f => IdentifierF f peerId clock -> Int -> Int
ithDigitDefault idf = fromMaybe 0 <<< ithDigit idf

ithPeerId :: forall f peerId clock. Container f => IdentifierF f peerId clock -> Int -> Maybe peerId
ithPeerId = ithElt peerId

ithClock :: forall f peerId clock. Container f => IdentifierF f peerId clock -> Int -> Maybe clock
ithClock = ithElt clock