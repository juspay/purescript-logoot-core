module Logoot.Types.Identifier where

import Prelude

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Logoot.Types.Position (Position, headPosition, lastPosition)
import Logoot.Types.Util (Base)

newtype IdentifierF f peerId clock = IdentifierF (f (Position peerId clock))

derive instance newtypeIdentifierF :: Newtype (IdentifierF f i c) _

derive instance genericIdentifierF :: Generic (IdentifierF f i c) _

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

instance encodeIdentifierF
  :: Encode (f (Position peerId clock))
  => Encode (IdentifierF f peerId clock) where
    encode = genericEncode defaultOptions {unwrapSingleConstructors = true}

instance decodeIdentifierF
  :: Decode (f (Position peerId clock))
  => Decode (IdentifierF f peerId clock) where
    decode = genericDecode defaultOptions {unwrapSingleConstructors = true}

-- | Suggested identifier that corresponds to the beginning of a document
headIdentifier :: forall f i c. Applicative f => IdentifierF f i c
headIdentifier = IdentifierF (pure headPosition)

-- | Suggested identifier that corresponds to the end of a document
lastIdentifier :: forall f i c. Applicative f => Base -> IdentifierF f i c
lastIdentifier = IdentifierF <<< pure <<< lastPosition