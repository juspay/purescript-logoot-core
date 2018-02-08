module Logoot.Types.Class.Site where

class Site s i c | s -> i c where
  siteId :: s -> i
  siteClock :: s -> c