module Logoot.Types.Class.Site where

import Prelude (class Monad)

class Site s m i c | s -> m i c where
  siteId :: Monad m => s -> m i
  siteClock :: Monad m => s -> m c