module Logoot.Types.Identifier where

import Prelude
import Logoot.Types.Position (Position)

newtype IdentifierF f peerId clock = Identifier (f (Position peerId clock))

derive instance eqIdentifierF
  :: (Eq (f (Position peerId clock)), Eq peerId, Eq clock)
  => Eq (IdentifierF f peerId clock)

derive instance ordIdentifierF
  :: (Ord (f (Position peerId clock)), Ord peerId, Ord clock)
  => Ord (IdentifierF f peerId clock)

instance showIdentifierF
  :: (Show (f (Position peerId clock)), Show peerId, Show clock)
  => Show (IdentifierF f peerId clock) where
    show (Identifier xs) = "(Identifier " <> show xs <> ")"
