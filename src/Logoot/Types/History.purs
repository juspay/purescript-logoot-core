-- | A convenience type for sending a history of commands to a client
-- | or parsing a history of commands from a server
module Logoot.Types.History where

import Prelude

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

newtype History a = History (Array a)

derive instance newtypeHistory :: Newtype (History a) _

derive instance genericHistory :: Generic (History a) _

derive newtype instance functorHistory :: Functor History

derive newtype instance eqHistory :: Eq a => Eq (History a)

instance showHistory :: Show a => Show (History a) where
  show (History hs) = "(History " <> show hs <> ")"

derive newtype instance applyHistory :: Apply History

derive newtype instance applicativeHistory :: Applicative History

derive newtype instance bindHistory :: Bind History

instance monadHistory :: Monad History

instance encodeHistory :: Encode a => Encode (History a) where
  encode x = genericEncode defaultOptions x

instance decodeHistory :: Decode a => Decode (History a) where
  decode x = genericDecode defaultOptions x