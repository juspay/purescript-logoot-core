module Data.Container where

import Prelude

import Control.Plus (class Plus)
import Data.Array as A
import Data.Foldable (class Foldable)
import Data.List as L
import Data.Maybe (Maybe)

class (Functor f, Foldable f, Plus f) <= Container f where
  take :: forall a. Int -> f a -> f a
  drop :: forall a. Int -> f a -> f a
  cons :: forall a. a -> f a -> f a
  index :: forall a. f a -> Int -> Maybe a
  length :: forall a. f a -> Int

instance containerArray :: Container Array where
  take = A.take
  drop = A.drop
  cons = A.cons
  index = A.index
  length = A.length

instance containerList :: Container L.List where
  take = L.take
  drop = L.drop
  cons = L.Cons
  index = L.index
  length = L.length

infixl 8 index as !!