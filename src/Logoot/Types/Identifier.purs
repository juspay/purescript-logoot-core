module Logoot.Types.Identifier where

import Prelude

import Data.Newtype (class Newtype)
import Logoot.Types.Position (Position)

newtype IdentifierF f peerId clock = IdentifierF (f (Position peerId clock))

derive instance newtypeIdentifierF :: Newtype (IdentifierF i s c) _

derive instance eqIdentifierF
  :: Eq (f (Position peerId clock))
  => Eq (IdentifierF f peerId clock)

derive instance ordIdentifierF
  :: Ord (f (Position peerId clock))
  => Ord (IdentifierF f peerId clock)

instance showIdentifierF
  :: Show (f (Position peerId clock))
  => Show (IdentifierF f peerId clock) where
    show (IdentifierF xs) = "(IdentifierF " <> show xs <> ")"
