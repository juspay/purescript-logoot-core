module Logoot.Types.Class.Site where

import Prelude (class Monad)

class Monad m <= Site s m i c | s -> m i c where
  siteId :: s -> m i
  siteClock :: s -> m c