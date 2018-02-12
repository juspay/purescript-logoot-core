module Data.Container where

import Prelude

import Control.Plus (class Alt, class Plus)
import Data.Array as A
import Data.Foldable (class Foldable)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.List as L
import Data.Maybe (Maybe)

class (Foldable f, Plus f, Alt f, Applicative f, FunctorWithIndex Int f) <= Container f where
  take :: Int -> f ~> f
  drop :: Int -> f ~> f
  cons :: forall a. a -> f a -> f a
  snoc :: forall a. f a -> a -> f a
  index :: forall a. f a -> Int -> Maybe a
  length :: forall a. f a -> Int
  reverse :: f ~> f
  last :: forall a. f a -> Maybe a

instance containerArray :: Container Array where
  take = A.take
  drop = A.drop
  cons = A.cons
  snoc = A.snoc
  index = A.index
  length = A.length
  reverse = A.reverse
  last = A.last

instance containerList :: Container L.List where
  take = L.take
  drop = L.drop
  cons = L.Cons
  snoc = L.snoc
  index = L.index
  length = L.length
  reverse = L.reverse
  last = L.last

infixl 8 index as !!

set :: forall f a. Container f => f a -> Int -> a -> f a
set xs i x = mapWithIndex f xs where
  f :: Int -> a -> a
  f n a
    | n == i = x
    | otherwise = a