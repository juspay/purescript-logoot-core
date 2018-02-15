module Logoot.Types.Position where

import Prelude
import Data.Maybe (Maybe)

data Position peerId clock = Position Int (Maybe peerId) (Maybe clock)

derive instance eqPosition :: (Eq peerId, Eq clock) => Eq (Position peerId clock)

derive instance ordPosition :: (Ord peerId, Ord clock) => Ord (Position peerId clock)

instance showPosition
  :: (Show peerId, Show clock)
  => Show (Position peerId clock) where
    show (Position i s c) =
      "(Position " <> show i <> " " <> show s <> " " <> show c <> ")"
