module Flex.Math.Minkowski
  ( Minkowski (Minkowski)
  , Scalar (..)
  )
  where

import Flex.Math.Module
import Flex.Math.Numbers

import Data.Foldable qualified as Data
import Data.Functor qualified as Data
import Data.Traversable qualified as Data
import Data.Eq (Eq)
import Data.Ord (Ord)
import Text.Show (Show)
import GHC.Generics (Generic)

data Minkowski x = Minkowski !x !x !x !x
  deriving
    ( Eq
    , Ord
    , Show
    , Data.Functor
    , Data.Foldable
    , Data.Traversable
    , Generic
    )

instance (Addition x x x) => Addition (Minkowski x) (Minkowski x) (Minkowski x) where
  (+.) :: Minkowski x -> Minkowski x -> Minkowski x
  Minkowski t0 x0 y0 z0 +. Minkowski t1 x1 y1 z1 =
    Minkowski (t0 + t1) (x0 + x1) (y0 + y1) (z0 + z1)
instance (Additive x) => Additive (Minkowski x) where
  zero :: Minkowski x
  zero = Minkowski zero zero zero zero
instance (AdditiveAbelian x) => AdditiveAbelian (Minkowski x)
instance (Subtraction x x x) => Subtraction (Minkowski x) (Minkowski x) (Minkowski x) where
  (-.) :: Minkowski x -> Minkowski x -> Minkowski x
  Minkowski t0 x0 y0 z0 -. Minkowski t1 x1 y1 z1 =
    Minkowski (t0 - t1) (x0 - x1) (y0 - y1) (z0 - z1)
instance (AdditiveGroup x) => AdditiveGroup (Minkowski x) where
  negative :: Minkowski x -> Minkowski x
  negative (Minkowski t x y z) = Minkowski (negative t) (negative x) (negative y) (negative z)

instance (Multiplication x x x) => Multiplication x (Minkowski x) (Minkowski x) where
  (*.) :: x -> Minkowski x -> Minkowski x
  k *. Minkowski t x y z = Minkowski (k * t) (k * x) (k * y) (k * z)
instance (Multiplication x x x) => Multiplication (Minkowski x) x (Minkowski x) where
  (*.) :: Minkowski x -> x -> Minkowski x
  Minkowski t x y z *. k = Minkowski (t * k) (x * k) (y * k) (z * k)
instance (Division x x x) => Division (Minkowski x) x (Minkowski x) where
  (/.) :: Minkowski x -> x -> Minkowski x
  Minkowski t x y z /. k = Minkowski (t /. k) (x /. k) (y /. k) (z /. k)
  
instance (Addition x x x) => Addition (Scalar (Minkowski x)) (Scalar (Minkowski x)) (Scalar (Minkowski x)) where
  (+.) :: Scalar (Minkowski x) -> Scalar (Minkowski x) -> Scalar (Minkowski x)
  ScalarMinkowski x +. ScalarMinkowski y = ScalarMinkowski (x + y)
instance (Subtraction x x x) => Subtraction (Scalar (Minkowski x)) (Scalar (Minkowski x)) (Scalar (Minkowski x)) where
  (-.) :: Scalar (Minkowski x) -> Scalar (Minkowski x) -> Scalar (Minkowski x)
  ScalarMinkowski x -. ScalarMinkowski y = ScalarMinkowski (x - y)
instance (Multiplication x x x) => Multiplication (Scalar (Minkowski x)) (Scalar (Minkowski x)) (Scalar (Minkowski x)) where
  (*.) :: Scalar (Minkowski x) -> Scalar (Minkowski x) -> Scalar (Minkowski x)
  ScalarMinkowski x *. ScalarMinkowski y = ScalarMinkowski (x * y)
instance (Division x x x) => Division (Scalar (Minkowski x)) (Scalar (Minkowski x)) (Scalar (Minkowski x)) where
  (/.) :: Scalar (Minkowski x) -> Scalar (Minkowski x) -> Scalar (Minkowski x)
  ScalarMinkowski x /. ScalarMinkowski y = ScalarMinkowski (x / y)
instance (Multiplication x x x) => Multiplication (Scalar (Minkowski x)) (Minkowski x) (Minkowski x) where
  (*.) :: Scalar (Minkowski x) -> Minkowski x -> Minkowski x
  ScalarMinkowski k *. m = k *. m
instance (Multiplication x x x) => Multiplication (Minkowski x) (Scalar (Minkowski x)) (Minkowski x) where
  (*.) :: Minkowski x -> Scalar (Minkowski x) -> Minkowski x
  m *. ScalarMinkowski k = m *. k
instance (Division x x x) => Division (Minkowski x) (Scalar (Minkowski x)) (Minkowski x) where
  (/.) :: Minkowski x -> Scalar (Minkowski x) -> Minkowski x
  m /. ScalarMinkowski k = m /. k
instance (Power x r x) => Power (Scalar (Minkowski x)) r (Scalar (Minkowski x)) where
  (^) :: Scalar (Minkowski x) -> r -> Scalar (Minkowski x)
  ScalarMinkowski x ^ r = ScalarMinkowski (x ^ r)
instance (Absolute x x) => Absolute (Scalar (Minkowski x)) x where
  absolute :: Scalar (Minkowski x) -> x
  absolute (ScalarMinkowski k) = absolute k
instance (Absolute x x) => Absolute (Scalar (Minkowski x)) (Scalar (Minkowski x)) where
  absolute :: Scalar (Minkowski x) -> Scalar (Minkowski x)
  absolute (ScalarMinkowski k) = ScalarMinkowski (absolute k)

instance (Semiring x) => Semiring (Scalar (Minkowski x))
instance (Ring x) => Ring (Scalar (Minkowski x))

instance (Ring x) => Module (Minkowski x) where
  newtype Scalar (Minkowski x) = ScalarMinkowski {unScalar :: x}
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
      , Domain
      , IntegralDomain
      , Field
      , Root
      , Generic
      )
instance (Field x) => Vector (Minkowski x)

instance (Ring x) => Bilinear (Minkowski x) where
  (•) :: Minkowski x -> Minkowski x -> Scalar (Minkowski x)
  Minkowski t0 x0 y0 z0 • Minkowski t1 x1 y1 z1 = ScalarMinkowski do
    (t0 * t1) - (x0 * x1) - (y0 * y1) - (z0 * z1)
