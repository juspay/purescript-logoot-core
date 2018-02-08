module Logoot.Types.Class.Clock where

import Prelude

class Clock c where
  cpp :: c -> c

instance clockInt :: Clock Int where
  cpp = (_ + 1)

instance clockNumber :: Clock Number where
  cpp = (_ + 1.0)