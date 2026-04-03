{-# LANGUAGE UndecidableInstances #-}

module Flex.Math.Foldable
  ( length
  , all
  , iall
  , any
  , iany
  , and
  , or
  , sum
  , sumOn
  , isumOn
  , sumWhen
  , isumWhen
  , product
  , productOn
  , iproductOn
  , productWhen
  , iproductWhen
  , count
  , icount
  , maximum
  , minimum
  , maximumOn
  , minimumOn
  , maximumOf
  , minimumOf
  ) where

import Flex.Math.Category
import Flex.Math.Numbers

import Data.Bool (Bool (..))
import Data.Function (const)
import Data.Semigroup
import Numeric.Natural (Natural)
import Data.Ord (Ord)

length :: (Foldable f) => f x -> Natural
length = count (const True)

all :: (Foldable f) => (x -> Bool) -> f x -> Bool
all x_b = getAll #. foldWith (All #. x_b)

iall :: (IxFoldable i f) => (i -> x -> Bool) -> f x -> Bool
iall i_x_b = getAll #. foldWith (Ix \i -> All #. i_x_b i)

any :: (Foldable f) => (x -> Bool) -> f x -> Bool
any x_b = getAny #. foldWith (Any #. x_b)

iany :: (IxFoldable i f) => (i -> x -> Bool) -> f x -> Bool
iany i_x_b = getAny #. foldWith (Ix \i -> Any #. i_x_b i)

and :: (Foldable f) => f Bool -> Bool
and = getAll #. foldWith All

or :: (Foldable f) => f Bool -> Bool
or = getAny #. foldWith Any

sum :: (Foldable f, AdditiveAbelian x) => f x -> x
sum = foldl (+) zero

sumOn :: (Foldable f, AdditiveAbelian y) => (x -> y) -> f x -> y
sumOn x_y = foldl (\y x -> y + x_y x) zero

isumOn ::
  (IxFoldable i f, AdditiveAbelian y) =>
  (i -> x -> y) -> f x -> y
isumOn p = ifoldl (\i acc x -> acc + p i x) zero

sumWhen ::
  forall t x. (AdditiveAbelian x, Foldable t) => (x -> Bool) -> t x -> x
sumWhen f = sumOn \x -> if f x then x else zero

isumWhen ::
  forall t x i.
  (AdditiveAbelian x, IxFoldable i t) => (i -> x -> Bool) -> t x -> x
isumWhen f = isumOn \i x -> if f i x then x else zero

product ::
  (Foldable f, MultiplicativeAbelian x) =>
  f x -> x
product = foldl (*) one

productOn ::
  (Foldable f, MultiplicativeAbelian y) =>
  (x -> y) -> f x -> y
productOn x_y = foldl (\y x -> y * x_y x) one

iproductOn ::
  (IxFoldable i f, MultiplicativeAbelian y) =>
  (i -> x -> y) -> f x -> y
iproductOn p = ifoldl (\i acc x -> acc * p i x) one

productWhen ::
  forall t x. (MultiplicativeAbelian x, Foldable t) => (x -> Bool) -> t x -> x
productWhen f = productOn \x -> if f x then x else one

iproductWhen ::
  forall t x i.
  (MultiplicativeAbelian x, IxFoldable i t) => (i -> x -> Bool) -> t x -> x
iproductWhen f = iproductOn \i x -> if f i x then x else one

count :: (Foldable f) => (x -> Bool) -> f x -> Natural
count p =
  foldl
    (\acc x -> acc + if p x then one @Natural else zero)
    zero

icount ::
  (IxFoldable i f) =>
  (i -> x -> Bool) -> f x -> Natural
icount p =
  ifoldl
    (\i acc x -> acc + if p i x then one @Natural else zero)
    zero

maximum :: (Foldable1 f, Ord x) => f x -> x
maximum = getMax . foldWith1 Max

minimum :: (Foldable1 f, Ord x) => f x -> x
minimum = getMin . foldWith1 Min

maximumOn :: (Foldable1 f, Ord y) => (x -> y) -> f x -> x
maximumOn x_y fx = case foldWith1 (Max . (Arg . x_y <*> id)) fx of
  Max (Arg _ x) -> x

minimumOn :: (Foldable1 f, Ord y) => (x -> y) -> f x -> x
minimumOn x_y fx = case foldWith1 (Min . (Arg . x_y <*> id)) fx of
  Min (Arg _ x) -> x

maximumOf :: (Foldable1 f, Ord y) => (x -> y) -> f x -> y
maximumOf x_y = getMax . foldWith1 (Max . x_y)

minimumOf :: (Foldable1 f, Ord y) => (x -> y) -> f x -> y
minimumOf x_y = getMin . foldWith1 (Min . x_y)
