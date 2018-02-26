module Logoot.Types.Util where

-- Utilities

import Data.Function (on)
import Data.Int as Z
import Data.Newtype (class Newtype)
import Math (pow)

raise :: Int -> Int -> Number
raise = pow `on` Z.toNumber

infixr 8 raise as ^

newtype Base = Base Int

derive instance newtypeBase :: Newtype Base _

newtype Boundary = Boundary Int

derive instance newtypeBoundary :: Newtype Boundary _