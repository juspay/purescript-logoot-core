module Logoot.Types
  ( module Exports
  , digit
  , peerId
  , clock
  ) where

import Logoot.Types.Position (Position(Position)) as Exports
import Logoot.Types.Identifier (IdentifierF(IdentifierF)) as Exports
import Logoot.Types.Position (Position(..))

digit :: forall peerId clock. Position peerId clock -> Int
digit (Position i _ _) = i

peerId :: forall peerId clock. Position peerId clock -> peerId
peerId (Position _ s _) = s

clock :: forall peerId clock. Position peerId clock -> clock
clock (Position _ _ c) = c