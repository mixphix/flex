{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Flex.Math.Polynomial
  ( Polynomial (Polynomial, unPolynomial)
  , fromList
  , toList
  , data X
  , (!)
  , monomial
  , derivative
  , Scalar (..)
  ) where

import Flex.Math.Category
import Flex.Math.Module
import Flex.Math.Numbers

import Control.Monad.ST.Strict
import Data.Bits (shiftR)
import Data.Bool
import Data.Enum
import Data.Eq
import Data.Function (fix)
import Data.List (List)
import Data.List qualified as List
import Data.Maybe
import Data.Ord
import Data.Vector.Strict qualified as Vector.Strict
import Data.Vector.Strict.Mutable qualified as Vector.Strict.Mutable
import GHC.Base (inline)
import GHC.Generics (Generic)
import Text.Show

newtype Polynomial x = Polynomial {unPolynomial :: Vector.Strict.Vector x}
  deriving newtype (Eq, Ord)

instance Morphisms (->) (->) Polynomial where
  morphism :: (x -> y) -> Polynomial x -> Polynomial y
  morphism x_y (Polynomial xs) = Polynomial (morphism x_y xs)
  {-# INLINE morphism #-}
instance Folds (->) (->) Polynomial where
  foldWith :: (Monoid z) => (x -> z) -> Polynomial x -> z
  foldWith x_z (Polynomial xs) = foldWith x_z xs
  {-# INLINE foldWith #-}
instance Traversals (->) (->) Polynomial where
  traverse :: (Applicative g) => (x -> g y) -> Polynomial x -> g (Polynomial y)
  traverse x_gy (Polynomial xs) = morphism Polynomial (traverse x_gy xs)
  {-# INLINE traverse #-}

fromList :: (Eq x, Additive x) => [x] -> Polynomial x
fromList xs = Polynomial case Vector.Strict.fromList xs of
  ys -> Vector.Strict.unsafeSlice 0 (reduce (Vector.Strict.length ys)) ys
   where
    reduce n
      | n == zero = zero
      | Vector.Strict.unsafeIndex ys (pred n) == zero = reduce (pred n)
      | otherwise = n
{-# INLINE fromList #-}

toList :: Polynomial x -> [x]
toList (Polynomial xs) = Vector.Strict.toList xs
{-# INLINE toList #-}

pattern X :: (Eq x, Additive x, Multiplicative x) => Polynomial x
pattern X <- ((Polynomial [zero, one] ==) -> True)
 where
  X = Polynomial [zero, one]

(!) :: (Additive x) => Polynomial x -> Natural -> x
Polynomial xs ! n = case xs Vector.Strict.!? from n of
  Nothing -> zero
  Just x -> x
{-# INLINE (!) #-}

monomial :: (Additive x) => Natural -> x -> Polynomial x
monomial j x = Polynomial do
  Vector.Strict.generate (from j + 1) \i -> if i == from j then x else zero
{-# INLINE monomial #-}

derivative ::
  (From Natural x, Multiplication x x x) => Polynomial x -> Polynomial x
derivative (Polynomial xs)
  | Vector.Strict.null xs = Polynomial []
  | otherwise = Polynomial do morphism (Ix \i x -> from @Natural (i + one) * x) (Vector.Strict.tail xs)

instance (Show x) => Show (Polynomial x) where
  showsPrec :: Int -> Polynomial x -> ShowS
  showsPrec prec (Polynomial xs)
    | Vector.Strict.null xs = showString "0"
    | Vector.Strict.length xs == 1 = showsPrec prec (Vector.Strict.head xs)
    | otherwise = showParen (prec > 0) do
        (foldl (.) id . List.intersperse (showString " + ")) do
          ifoldr @Natural (\i x acc -> showCoeff i x : acc) [] xs
   where
    showCoeff = \case
      0 -> showsPrec 7
      1 -> \x -> showsPrec 7 x . showString "x"
      n -> \x -> showsPrec 7 x . showString ("x^" <> show n)

instance (Addition x x x) => Addition (Polynomial x) (Polynomial x) (Polynomial x) where
  (+.) :: Polynomial x -> Polynomial x -> Polynomial x
  Polynomial xs +. Polynomial ys = runST do
    let !x = Vector.Strict.length xs
        !y = Vector.Strict.length ys
        lo = min x y
        hi = max x y
    zs <- Vector.Strict.Mutable.unsafeNew hi
    for_ @List [0 .. lo - 1] \i ->
      Vector.Strict.Mutable.unsafeWrite zs i do
        Vector.Strict.unsafeIndex xs i + Vector.Strict.unsafeIndex ys i
    Vector.Strict.unsafeCopy
      (Vector.Strict.Mutable.unsafeSlice lo (hi - lo) zs)
      (Vector.Strict.unsafeSlice lo (hi - lo) (if x <= y then ys else xs))
    morphism Polynomial do
      Vector.Strict.unsafeFreeze zs
  {-# INLINEABLE (+.) #-}
instance (Additive x) => Additive (Polynomial x) where
  zero :: Polynomial x
  zero = Polynomial Vector.Strict.empty
instance (AdditiveAbelian x) => AdditiveAbelian (Polynomial x)
instance
  (Additive x, Subtraction x x x) =>
  Subtraction (Polynomial x) (Polynomial x) (Polynomial x)
  where
  (-.) :: Polynomial x -> Polynomial x -> Polynomial x
  Polynomial xs -. Polynomial ys = runST do
    let !x = Vector.Strict.length xs
        !y = Vector.Strict.length ys
        lo = min x y
        hi = max x y
    zs <- Vector.Strict.Mutable.unsafeNew hi
    for_ @List [0 .. lo - 1] \i ->
      Vector.Strict.Mutable.unsafeWrite zs i do
        Vector.Strict.unsafeIndex xs i - Vector.Strict.unsafeIndex ys i
    if x < y
      then for_ @List [x .. y - 1] \i ->
        Vector.Strict.Mutable.unsafeWrite zs i (zero - Vector.Strict.unsafeIndex ys i)
      else do
        Vector.Strict.unsafeCopy
          (Vector.Strict.Mutable.unsafeSlice y (x - y) zs)
          (Vector.Strict.unsafeSlice y (x - y) xs)
    morphism Polynomial do
      Vector.Strict.unsafeFreeze zs
  {-# INLINEABLE (-.) #-}
instance (AdditiveGroup x) => AdditiveGroup (Polynomial x) where
  negative :: Polynomial x -> Polynomial x
  negative (Polynomial xs) = Polynomial (morphism negative xs)
  {-# INLINE negative #-}

convolution ::
  (Additive x, Multiplication x x x) =>
  Vector.Strict.Vector x -> Vector.Strict.Vector x -> Vector.Strict.Vector x
convolution xs ys = do
  let !x = Vector.Strict.length xs
      !y = Vector.Strict.length ys
      !z = x + y - 1
  if List.elem @List zero [x, y]
    then Vector.Strict.empty
    else Vector.Strict.generate z \i ->
      foldl @List
        ( \acc j -> acc + Vector.Strict.unsafeIndex xs j * Vector.Strict.unsafeIndex ys (i - j)
        )
        zero
        [max (i - y + 1) 0 .. min i (x - 1)]
{-# INLINEABLE convolution #-}

karatsuba ::
  forall x.
  (Additive x, Subtraction x x x, Multiplication x x x) =>
  Vector.Strict.Vector x -> Vector.Strict.Vector x -> Vector.Strict.Vector x
karatsuba = fix \rec xs ys -> do
  let !x = Vector.Strict.length xs
      !y = Vector.Strict.length ys
      !m = (min x y + 1) `shiftR` 1

      xs0 = Vector.Strict.slice 0 m xs
      xs1 = Vector.Strict.slice m (x - m) xs
      ys0 = Vector.Strict.slice 0 m ys
      ys1 = Vector.Strict.slice m (y - m) ys

      Polynomial xs01 = Polynomial xs0 + Polynomial xs1
      Polynomial ys01 = Polynomial ys0 + Polynomial ys1

      conv = inline (convolution @x)
  if x <= 32 || y <= 32
    then conv xs ys
    else runST do
      let zs0 = rec xs0 ys0
          zs2 = rec xs1 ys1
          zs11 = rec xs01 ys01
      zs <- Vector.Strict.Mutable.unsafeNew (x + y - 1)
      for_ @List [0 .. x + y - 2] \k -> do
        let z0 =
              if k < Vector.Strict.length zs0
                then Vector.Strict.unsafeIndex zs0 k
                else zero
            z11 =
              if k - m >= 0 && k - m < Vector.Strict.length zs11
                then Vector.Strict.unsafeIndex zs11 (k - m)
                else zero
            z10 =
              if k - m >= 0 && k - m < Vector.Strict.length zs0
                then Vector.Strict.unsafeIndex zs0 (k - m)
                else zero
            z12 =
              if k - m >= 0 && k - m < Vector.Strict.length zs2
                then Vector.Strict.unsafeIndex zs2 (k - m)
                else zero
            z2 =
              if k - 2 * m >= 0 && k - 2 * m < Vector.Strict.length zs2
                then Vector.Strict.unsafeIndex zs2 (k - 2 * m)
                else zero
        Vector.Strict.Mutable.unsafeWrite zs k (z0 + (z11 - (z10 + z12)) + z2)
      Vector.Strict.unsafeFreeze zs
{-# INLINEABLE karatsuba #-}

instance
  (Additive x, Subtraction x x x, Multiplication x x x) =>
  Multiplication (Polynomial x) (Polynomial x) (Polynomial x)
  where
  (*.) :: Polynomial x -> Polynomial x -> Polynomial x
  Polynomial xs *. Polynomial ys = Polynomial do inline karatsuba xs ys
instance
  (Additive x, Subtraction x x x, Multiplicative x) =>
  Multiplicative (Polynomial x)
  where
  one :: Polynomial x
  one = Polynomial (Vector.Strict.singleton one)
instance
  (Additive x, Subtraction x x x, MultiplicativeAbelian x) =>
  MultiplicativeAbelian (Polynomial x)

instance (Multiplication x x x) => Multiplication x (Polynomial x) (Polynomial x) where
  (*.) :: x -> Polynomial x -> Polynomial x
  k *. Polynomial xs = Polynomial (morphism (k *) xs)
instance (Multiplication x x x) => Multiplication (Polynomial x) x (Polynomial x) where
  (*.) :: Polynomial x -> x -> Polynomial x
  Polynomial xs *. k = Polynomial (morphism (* k) xs)
instance (Division x x x) => Division (Polynomial x) x (Polynomial x) where
  (/.) :: Polynomial x -> x -> Polynomial x
  Polynomial xs /. k = Polynomial (morphism (/ k) xs)

instance (Additive x, Subtraction x x x, Multiplicative x) => Distributive (Polynomial x)
instance (Additive x, Subtraction x x x, Multiplicative x) => Semiring (Polynomial x)
instance (AdditiveAbelianGroup x, Multiplicative x) => Ring (Polynomial x)

instance (From y x) => From y (Scalar (Polynomial x)) where
  from :: y -> Scalar (Polynomial x)
  from n = ScalarPoly (from n)
  {-# INLINE from #-}

instance
  (Addition x x x) =>
  Addition (Scalar (Polynomial x)) (Scalar (Polynomial x)) (Scalar (Polynomial x))
  where
  (+.) :: Scalar (Polynomial x) -> Scalar (Polynomial x) -> Scalar (Polynomial x)
  ScalarPoly x +. ScalarPoly y = ScalarPoly (x + y)
  {-# INLINE (+.) #-}
instance
  (Subtraction x x x) =>
  Subtraction
    (Scalar (Polynomial x))
    (Scalar (Polynomial x))
    (Scalar (Polynomial x))
  where
  (-.) :: Scalar (Polynomial x) -> Scalar (Polynomial x) -> Scalar (Polynomial x)
  ScalarPoly x -. ScalarPoly y = ScalarPoly (x - y)
  {-# INLINE (-.) #-}
instance
  (Multiplication x x x) =>
  Multiplication
    (Scalar (Polynomial x))
    (Scalar (Polynomial x))
    (Scalar (Polynomial x))
  where
  (*.) :: Scalar (Polynomial x) -> Scalar (Polynomial x) -> Scalar (Polynomial x)
  ScalarPoly x *. ScalarPoly y = ScalarPoly (x * y)
  {-# INLINE (*.) #-}
instance
  (Division x x x) =>
  Division (Scalar (Polynomial x)) (Scalar (Polynomial x)) (Scalar (Polynomial x))
  where
  (/.) :: Scalar (Polynomial x) -> Scalar (Polynomial x) -> Scalar (Polynomial x)
  ScalarPoly x /. ScalarPoly y = ScalarPoly (x / y)
  {-# INLINE (/.) #-}
instance
  (Eq x, Additive x, Multiplicative x) =>
  Multiplication (Scalar (Polynomial x)) (Polynomial x) (Polynomial x)
  where
  (*.) :: Scalar (Polynomial x) -> Polynomial x -> Polynomial x
  ScalarPoly k *. x = k *. x
  {-# INLINE (*.) #-}
instance
  (Eq x, Additive x, Multiplicative x) =>
  Multiplication (Polynomial x) (Scalar (Polynomial x)) (Polynomial x)
  where
  (*.) :: Polynomial x -> Scalar (Polynomial x) -> Polynomial x
  x *. ScalarPoly k = x *. k
  {-# INLINE (*.) #-}
instance
  (Eq x, Additive x, Division x x x) =>
  Division (Polynomial x) (Scalar (Polynomial x)) (Polynomial x)
  where
  (/.) :: Polynomial x -> Scalar (Polynomial x) -> Polynomial x
  x /. ScalarPoly k = x /. k
  {-# INLINE (/.) #-}
instance (Power x r x) => Power (Scalar (Polynomial x)) r (Scalar (Polynomial x)) where
  (^) :: Scalar (Polynomial x) -> r -> Scalar (Polynomial x)
  ScalarPoly x ^ r = ScalarPoly (x ^ r)
  {-# INLINE (^) #-}
instance (Absolute x x) => Absolute (Scalar (Polynomial x)) x where
  absolute :: Scalar (Polynomial x) -> x
  absolute (ScalarPoly k) = absolute k
  {-# INLINE absolute #-}
instance (Absolute x x) => Absolute (Scalar (Polynomial x)) (Scalar (Polynomial x)) where
  absolute :: Scalar (Polynomial x) -> Scalar (Polynomial x)
  absolute (ScalarPoly k) = ScalarPoly (absolute k)
  {-# INLINE absolute #-}

instance
  (Additive x, Subtraction x x x, Multiplicative x) =>
  Semiring (Scalar (Polynomial x))
instance
  (AdditiveAbelian x, AdditiveGroup x, Multiplicative x) =>
  Ring (Scalar (Polynomial x))
instance (Domain x) => Domain (Scalar (Polynomial x))

instance (Ring x, AdditiveAbelian x, Eq x) => Module (Polynomial x) where
  newtype Scalar (Polynomial x) = ScalarPoly {unScalar :: x}
    deriving newtype
      ( Eq
      , Ord
      , Show
      , Signed
      , Conjugate
      , Additive
      , AdditiveAbelian
      , AdditiveGroup
      , Multiplicative
      , MultiplicativeAbelian
      , MultiplicativeGroup
      , Distributive
      , IntegralDomain
      , Field
      , Root
      , Generic
      )
instance (Eq x, Field x) => Vector (Polynomial x)
