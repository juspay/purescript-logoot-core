module Logoot.Types.Class.Site where

import Data.Maybe (Maybe)
import Prelude (class Monad)

class Site s m i c | s -> m i c where
  siteId :: Monad m => s -> m (Maybe i)
  siteClock :: Monad m => s -> m (Maybe c)