module Data.Container where

import Prelude

import Control.Plus (class Plus)
import Data.Tuple (Tuple)
import Data.Array as A
import Data.Foldable (class Foldable)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List as L
import Data.Maybe (Maybe)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Unfoldable (replicate) as L'

class
  ( Foldable f
  , Plus f
  , Applicative f
  , TraversableWithIndex Int f
  ) <= Container f where
  take :: Int -> f ~> f
  drop :: Int -> f ~> f
  cons :: forall a. a -> f a -> f a
  index :: forall a. f a -> Int -> Maybe a
  reverse :: f ~> f
  last :: forall a. f a -> Maybe a
  dropWhile :: forall a. (a -> Boolean) -> f a -> f a
  findIndex :: forall a. (a -> Boolean) -> f a -> Maybe Int
  replicate :: forall a. Int -> a -> f a
  insert :: forall a. Ord a => a -> f a -> f a
  slice :: Int -> Int -> f ~> f
  zip :: forall a b. f a -> f b -> f (Tuple a b)
  zipWith :: forall a b c. (a -> b -> c) -> f a -> f b -> f c
  snoc :: forall a. f a -> a -> f a
  fromFoldable :: forall g. Foldable g => g ~> f
  -- length :: forall a. f a -> Int

instance containerArray :: Container Array where
  take = A.take
  drop = A.drop
  cons = A.cons
  index = A.index
  reverse = A.reverse
  last = A.last
  dropWhile = A.dropWhile
  findIndex = A.findIndex
  replicate = A.replicate
  insert = A.insert
  slice = A.slice
  zip = A.zip
  zipWith = A.zipWith
  snoc = A.snoc
  fromFoldable = A.fromFoldable
  -- length = A.length

instance containerList :: Container L.List where
  take = L.take
  drop = L.drop
  cons = L.Cons
  index = L.index
  reverse = L.reverse
  last = L.last
  dropWhile = L.dropWhile
  findIndex = L.findIndex
  replicate = L'.replicate
  insert = L.insert
  slice = L.slice
  zip = L.zip
  zipWith = L.zipWith
  snoc = L.snoc
  fromFoldable = L.fromFoldable
  -- length = L.length

infixl 8 index as !!

set :: forall f a. Container f => f a -> Int -> a -> f a
set xs i x = mapWithIndex f xs where
  f :: Int -> a -> a
  f n a
    | n == i = x
    | otherwise = a
