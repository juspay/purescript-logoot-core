module Logoot.Types.Position
  ( Position(..)
  , headPosition
  , lastPosition
  ) where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), unNullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Logoot.Types.Util (Base(..))

data Position peerId clock = Position Int (Maybe peerId) (Maybe clock)

derive instance eqPosition :: (Eq peerId, Eq clock) => Eq (Position peerId clock)

derive instance ordPosition :: (Ord peerId, Ord clock) => Ord (Position peerId clock)

derive instance genericPosition :: Generic (Position peerId clock) _

instance showPosition
  :: (Show peerId, Show clock)
  => Show (Position peerId clock) where
    show (Position i s c) =
      "(Position " <> show i <> " " <> show s <> " " <> show c <> ")"

instance bifunctorPosition :: Bifunctor Position where
  bimap l r (Position i s c) = Position i (l <$> s) (r <$> c)

instance encodePosition :: (Encode i, Encode c) => Encode (Position i c) where
  encode = encode <<< toPositionJSON

instance decodePosition :: (Decode i, Decode c) => Decode (Position i c) where
  decode = map fromPositionJSON <<< decode

data PositionJSON i c = PositionJSON Int (NullOrUndefined i) (NullOrUndefined c)

toPositionJSON :: forall i c. Position i c -> PositionJSON i c
toPositionJSON (Position s i c) = PositionJSON s (NullOrUndefined i) (NullOrUndefined c)

fromPositionJSON :: forall i c. PositionJSON i c -> Position i c
fromPositionJSON (PositionJSON s i c) = Position s (unNullOrUndefined i) (unNullOrUndefined c)

derive instance genericPositionJSON :: Generic (PositionJSON i c) _

instance encodePositionJSON :: (Encode i, Encode c) => Encode (PositionJSON i c) where
  encode = genericEncode defaultOptions {unwrapSingleConstructors = true}

instance decodePositionJSON :: (Decode i, Decode c) => Decode (PositionJSON i c) where
  decode = genericDecode defaultOptions {unwrapSingleConstructors = true}

-- | Suggested position that corresponds to the beginning of a document
headPosition :: forall i c. Position i c
headPosition = Position 0 Nothing Nothing

-- | Suggested position that corresponds to the end of a document.
lastPosition :: forall i c. Base -> Position i c
lastPosition (Base b) = Position b Nothing Nothing