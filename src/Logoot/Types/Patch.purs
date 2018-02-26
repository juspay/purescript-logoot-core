module Logoot.Types.Patch where

import Prelude

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Logoot.Types.Identifier (IdentifierF)

newtype Patch f g s c = Patch (f (IdentifierF g s c))

derive instance genericPatch :: Generic (Patch f g s c) _

derive instance eqPatch
  :: Eq (f (IdentifierF g s c))
  => Eq (Patch f g s c)

derive instance ordPatch
  :: Ord (f (IdentifierF g s c))
  => Ord (Patch f g s c)

instance showPatch
  :: Show (f (IdentifierF g s c))
  => Show (Patch f g s c) where
    show (Patch p) = "(Patch " <> show p <> ")"

instance encodePatch
  :: Encode (f (IdentifierF g s c))
  => Encode (Patch f g s c) where
    encode = genericEncode defaultOptions {unwrapSingleConstructors = true}

instance decodePatch
  :: Decode (f (IdentifierF g s c))
  => Decode (Patch f g s c) where
    decode = genericDecode defaultOptions {unwrapSingleConstructors = true}

type HPatch f s c = Patch f f s c