module Logoot.Types.Patch where

import Prelude
import Logoot.Types.Identifier (IdentifierF)

newtype Patch f g s c = Patch (f (IdentifierF g s c))

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

type HPatch f s c = Patch f f s c