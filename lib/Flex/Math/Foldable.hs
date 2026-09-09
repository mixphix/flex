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
import Data.Ord (Ord)
import Data.Semigroup

length :: (Foldable f) => f x -> Natural
length = count (const True)
{-# INLINE length #-}

all :: (Foldable f) => (x -> Bool) -> f x -> Bool
all x_b = getAll #. foldWith (All #. x_b)
{-# INLINE all #-}

iall :: (IxFoldable i f) => (i -> x -> Bool) -> f x -> Bool
iall i_x_b = getAll #. foldWith (Ix \i -> All #. i_x_b i)
{-# INLINE iall #-}

any :: (Foldable f) => (x -> Bool) -> f x -> Bool
any x_b = getAny #. foldWith (Any #. x_b)
{-# INLINE any #-}

iany :: (IxFoldable i f) => (i -> x -> Bool) -> f x -> Bool
iany i_x_b = getAny #. foldWith (Ix \i -> Any #. i_x_b i)
{-# INLINE iany #-}

and :: (Foldable f) => f Bool -> Bool
and = getAll #. foldWith All
{-# INLINE and #-}

or :: (Foldable f) => f Bool -> Bool
or = getAny #. foldWith Any
{-# INLINE or #-}

sum :: (Foldable f, AdditiveAbelian x) => f x -> x
sum = foldl (+) zero
{-# INLINE sum #-}

sumOn :: (Foldable f, AdditiveAbelian y) => (x -> y) -> f x -> y
sumOn x_y = foldl (\y x -> y + x_y x) zero
{-# INLINE sumOn #-}

isumOn ::
  (IxFoldable i f, AdditiveAbelian y) =>
  (i -> x -> y) -> f x -> y
isumOn p = ifoldl (\i acc x -> acc + p i x) zero
{-# INLINE isumOn #-}

sumWhen ::
  forall t x. (AdditiveAbelian x, Foldable t) => (x -> Bool) -> t x -> x
sumWhen f = sumOn \x -> if f x then x else zero
{-# INLINE sumWhen #-}

isumWhen ::
  forall t x i.
  (AdditiveAbelian x, IxFoldable i t) => (i -> x -> Bool) -> t x -> x
isumWhen f = isumOn \i x -> if f i x then x else zero
{-# INLINE isumWhen #-}

product ::
  (Foldable f, MultiplicativeAbelian x) =>
  f x -> x
product = foldl (*) one
{-# INLINE product #-}

productOn ::
  (Foldable f, MultiplicativeAbelian y) =>
  (x -> y) -> f x -> y
productOn x_y = foldl (\y x -> y * x_y x) one
{-# INLINE productOn #-}

iproductOn ::
  (IxFoldable i f, MultiplicativeAbelian y) =>
  (i -> x -> y) -> f x -> y
iproductOn p = ifoldl (\i acc x -> acc * p i x) one
{-# INLINE iproductOn #-}

productWhen ::
  forall t x. (MultiplicativeAbelian x, Foldable t) => (x -> Bool) -> t x -> x
productWhen f = productOn \x -> if f x then x else one
{-# INLINE productWhen #-}

iproductWhen ::
  forall t x i.
  (MultiplicativeAbelian x, IxFoldable i t) => (i -> x -> Bool) -> t x -> x
iproductWhen f = iproductOn \i x -> if f i x then x else one
{-# INLINE iproductWhen #-}

count :: (Foldable f) => (x -> Bool) -> f x -> Natural
count p =
  foldl
    (\acc x -> acc + if p x then one @Natural else zero)
    zero
{-# INLINE count #-}

icount ::
  (IxFoldable i f) =>
  (i -> x -> Bool) -> f x -> Natural
icount p =
  ifoldl
    (\i acc x -> acc + if p i x then one @Natural else zero)
    zero
{-# INLINE icount #-}

maximum :: (Foldable1 f, Ord x) => f x -> x
maximum = getMax . foldWith1 Max
{-# INLINE maximum #-}

minimum :: (Foldable1 f, Ord x) => f x -> x
minimum = getMin . foldWith1 Min
{-# INLINE minimum #-}

maximumOn :: (Foldable1 f, Ord y) => (x -> y) -> f x -> x
maximumOn x_y fx = case foldWith1 (Max . (Arg . x_y <*> id)) fx of
  Max (Arg _ x) -> x
{-# INLINE maximumOn #-}

minimumOn :: (Foldable1 f, Ord y) => (x -> y) -> f x -> x
minimumOn x_y fx = case foldWith1 (Min . (Arg . x_y <*> id)) fx of
  Min (Arg _ x) -> x
{-# INLINE minimumOn #-}

maximumOf :: (Foldable1 f, Ord y) => (x -> y) -> f x -> y
maximumOf x_y = getMax . foldWith1 (Max . x_y)
{-# INLINE maximumOf #-}

minimumOf :: (Foldable1 f, Ord y) => (x -> y) -> f x -> y
minimumOf x_y = getMin . foldWith1 (Min . x_y)
{-# INLINE minimumOf #-}
