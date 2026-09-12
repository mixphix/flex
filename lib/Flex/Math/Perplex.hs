module Flex.Math.Perplex
  ( Perplex ((:!))
  , PerplexBasis (Simple, Perplex)
  , simple
  , perplex
  , diagonal
  , undiagonal
  , Scalar (..)
  )
where

import Flex.Math.Algebra
import Flex.Math.Category
import Flex.Math.Module
import Flex.Math.Numbers

import Data.Bounded (Bounded)
import Data.Enum (Enum)
import Data.Eq (Eq (..))
import Data.Foldable qualified as Data
import Data.Foldable1 qualified as Data
import Data.Functor qualified as Data
import Data.Ord (Ord (..))
import Data.Traversable qualified as Data
import GHC.Generics (Generic)
import Text.Read (Read)
import Text.Show (Show)

infixl 4 :!
data Perplex x = !x :! !x
  deriving
    ( Eq
    , Ord
    , Show
    , Read
    , Data.Functor
    , Data.Foldable
    , Data.Traversable
    )

data PerplexBasis
  = Simple
  | Perplex
  deriving (Eq, Ord, Enum, Bounded)

simple :: Perplex x -> x
simple (x :! _) = x
{-# INLINE simple #-}

perplex :: Perplex x -> x
perplex (_ :! x) = x
{-# INLINE perplex #-}

diagonal :: (Additive x, Subtraction x x x) => Perplex x -> (x, x)
diagonal (x :! y) = (x - y, x + y)
{-# INLINE diagonal #-}

undiagonal ::
  (Additive x, Subtraction x x x, MultiplicativeGroup x) => (x, x) -> Perplex x
undiagonal (i, j) = (i + j) / (one + one) :! (j - i) / (one + one)
{-# INLINE undiagonal #-}

instance Morphisms (->) (->) Perplex where
  morphism :: (x -> y) -> Perplex x -> Perplex y
  morphism = Data.fmap
  {-# INLINE morphism #-}
instance Folds (->) (->) Perplex where
  foldWith :: (Monoid z) => (x -> z) -> Perplex x -> z
  foldWith x_z (xp :! xt) = x_z xp <> x_z xt
  {-# INLINE foldWith #-}
instance Folds1 (->) (->) Perplex where
  foldWith1 :: (Semigroup z) => (x -> z) -> Perplex x -> z
  foldWith1 x_z (xp :! xt) = x_z xp <> x_z xt
  {-# INLINE foldWith1 #-}
instance Traversals (->) (->) Perplex where
  traverse :: (Applicative g) => (x -> g y) -> Perplex x -> g (Perplex y)
  traverse x_gy (xp :! xt) = liftA2 (:!) (x_gy xp) (x_gy xt)
  {-# INLINE traverse #-}
instance Traversals1 (->) (->) Perplex where
  traverse1 :: (Apply g) => (x -> g y) -> Perplex x -> g (Perplex y)
  traverse1 x_gy (xp :! xt) = liftA2 (:!) (x_gy xp) (x_gy xt)
  {-# INLINE traverse1 #-}
instance Morphisms (Ix PerplexBasis) (->) Perplex where
  morphism :: Ix PerplexBasis x y -> Perplex x -> Perplex y
  morphism (Ix e_x_y) (xp :! xt) =
    e_x_y Simple xp :! e_x_y Perplex xt
  {-# INLINE morphism #-}
instance Folds (Ix PerplexBasis) (->) Perplex where
  foldWith :: (Monoid z) => Ix PerplexBasis x z -> Perplex x -> z
  foldWith (Ix e_x_z) (xp :! xt) = e_x_z Simple xp <> e_x_z Perplex xt
  {-# INLINE foldWith #-}
instance Traversals (Ix PerplexBasis) (->) Perplex where
  traverse ::
    (Applicative g) => Ix PerplexBasis x (g y) -> Perplex x -> g (Perplex y)
  traverse (Ix e_x_gy) (xp :! xt) = liftA2 (:!) (e_x_gy Simple xp) (e_x_gy Perplex xt)
  {-# INLINE traverse #-}
instance Folds1 (Ix PerplexBasis) (->) Perplex where
  foldWith1 :: (Semigroup z) => Ix PerplexBasis x z -> Perplex x -> z
  foldWith1 (Ix e_x_z) (xp :! xt) = e_x_z Simple xp <> e_x_z Perplex xt
  {-# INLINE foldWith1 #-}
instance Traversals1 (Ix PerplexBasis) (->) Perplex where
  traverse1 ::
    (Apply g) => Ix PerplexBasis x (g y) -> Perplex x -> g (Perplex y)
  traverse1 (Ix e_x_gy) (xp :! xt) = liftA2 (:!) (e_x_gy Simple xp) (e_x_gy Perplex xt)
  {-# INLINE traverse1 #-}
instance Apply Perplex where
  (<*>) :: Perplex (x -> y) -> Perplex x -> Perplex y
  (py :! ty) <*> (xp :! xt) = py xp :! ty xt
  {-# INLINE (<*>) #-}
instance Collectable Perplex where
  distribute :: (Along f) => f (Perplex x) -> Perplex (f x)
  distribute fd = morphism simple fd :! morphism perplex fd
  {-# INLINE distribute #-}
instance Tabulation Perplex where
  type Table Perplex = PerplexBasis
  fromTable :: (Table Perplex -> a) -> Perplex a
  fromTable f = f Simple :! f Perplex
  {-# INLINE fromTable #-}
  toTable :: Perplex a -> Table Perplex -> a
  toTable (r :! i) = \case
    Simple -> r
    Perplex -> i
  {-# INLINE toTable #-}
instance Data.Foldable1 Perplex where
  foldMap1 :: (Semigroup m) => (x -> m) -> Perplex x -> m
  foldMap1 x_m (xp :! xt) = x_m xp <> x_m xt
  {-# INLINE foldMap1 #-}

instance (Additive y, From y x) => From y (Perplex x) where
  from :: y -> Perplex x
  from y = from y :! from @y zero
  {-# INLINE from #-}

instance (Addition x x x) => Addition (Perplex x) (Perplex x) (Perplex x) where
  (+.) :: Perplex x -> Perplex x -> Perplex x
  (x :! x') +. (y :! y') = (x + y) :! (x' + y')
  {-# INLINE (+.) #-}
instance (Additive x) => Additive (Perplex x) where
  zero :: Perplex x
  zero = zero :! zero
  {-# INLINE zero #-}
instance (AdditiveAbelian x) => AdditiveAbelian (Perplex x)
instance (Subtraction x x x) => Subtraction (Perplex x) (Perplex x) (Perplex x) where
  (-.) :: Perplex x -> Perplex x -> Perplex x
  (x :! x') -. (y :! y') = (x - y) :! (x' - y')
  {-# INLINE (-.) #-}
instance (AdditiveGroup x) => AdditiveGroup (Perplex x) where
  negative :: (AdditiveGroup x) => Perplex x -> Perplex x
  negative (x :! x') = negative x :! negative x'
  {-# INLINE negative #-}

instance (AdditiveGroup x) => Conjugate (Perplex x) where
  conjugate :: Perplex x -> Perplex x
  conjugate (x :! x') = x :! negative x'
  {-# INLINE conjugate #-}

instance
  (Addition x x x, Multiplication x x x) =>
  Multiplication (Perplex x) (Perplex x) (Perplex x)
  where
  (*.) :: Perplex x -> Perplex x -> Perplex x
  (x :! y) *. (u :! v) = (x * u + y * v) :! (x * v + y * u)
  {-# INLINE (*.) #-}
instance (Additive x, Multiplicative x) => Multiplicative (Perplex x) where
  one :: Perplex x
  one = one :! zero
  {-# INLINE one #-}
instance (Additive x, MultiplicativeAbelian x) => MultiplicativeAbelian (Perplex x)

instance (Multiplication x x x) => Multiplication x (Perplex x) (Perplex x) where
  (*.) :: x -> Perplex x -> Perplex x
  k *. (x :! x') = k * x :! k * x'
  {-# INLINE (*.) #-}
instance (Multiplication x x x) => Multiplication (Perplex x) x (Perplex x) where
  (*.) :: Perplex x -> x -> Perplex x
  (x :! x') *. k = x * k :! x' * k
  {-# INLINE (*.) #-}
instance (Division x x x) => Division (Perplex x) x (Perplex x) where
  (/.) :: Perplex x -> x -> Perplex x
  (x :! x') /. k = x / k :! x' / k
  {-# INLINE (/.) #-}

instance (From y x) => From y (Scalar (Perplex x)) where
  from :: y -> Scalar (Perplex x)
  from n = ScalarPerplex (from n)
  {-# INLINE from #-}

instance
  (Addition x x x) =>
  Addition (Scalar (Perplex x)) (Scalar (Perplex x)) (Scalar (Perplex x))
  where
  (+.) :: Scalar (Perplex x) -> Scalar (Perplex x) -> Scalar (Perplex x)
  ScalarPerplex x +. ScalarPerplex y = ScalarPerplex (x + y)
  {-# INLINE (+.) #-}
instance
  (Subtraction x x x) =>
  Subtraction (Scalar (Perplex x)) (Scalar (Perplex x)) (Scalar (Perplex x))
  where
  (-.) :: Scalar (Perplex x) -> Scalar (Perplex x) -> Scalar (Perplex x)
  ScalarPerplex x -. ScalarPerplex y = ScalarPerplex (x - y)
  {-# INLINE (-.) #-}
instance
  (Multiplication x x x) =>
  Multiplication (Scalar (Perplex x)) (Scalar (Perplex x)) (Scalar (Perplex x))
  where
  (*.) :: Scalar (Perplex x) -> Scalar (Perplex x) -> Scalar (Perplex x)
  ScalarPerplex x *. ScalarPerplex y = ScalarPerplex (x * y)
  {-# INLINE (*.) #-}
instance
  (Division x x x) =>
  Division (Scalar (Perplex x)) (Scalar (Perplex x)) (Scalar (Perplex x))
  where
  (/.) :: Scalar (Perplex x) -> Scalar (Perplex x) -> Scalar (Perplex x)
  ScalarPerplex x /. ScalarPerplex y = ScalarPerplex (x / y)
  {-# INLINE (/.) #-}
instance
  (Multiplication x x x) =>
  Multiplication (Scalar (Perplex x)) (Perplex x) (Perplex x)
  where
  (*.) :: Scalar (Perplex x) -> Perplex x -> Perplex x
  ScalarPerplex k *. (x :! x') = k * x :! k * x'
  {-# INLINE (*.) #-}
instance
  (Multiplication x x x) =>
  Multiplication (Perplex x) (Scalar (Perplex x)) (Perplex x)
  where
  (*.) :: Perplex x -> Scalar (Perplex x) -> Perplex x
  (x :! x') *. ScalarPerplex k = x * k :! x' * k
  {-# INLINE (*.) #-}
instance (Division x x x) => Division (Perplex x) (Scalar (Perplex x)) (Perplex x) where
  (/.) :: Perplex x -> Scalar (Perplex x) -> Perplex x
  (x :! x') /. ScalarPerplex k = x / k :! x' / k
  {-# INLINE (/.) #-}
instance (Power x r x) => Power (Scalar (Perplex x)) r (Scalar (Perplex x)) where
  (^) :: Scalar (Perplex x) -> r -> Scalar (Perplex x)
  ScalarPerplex x ^ r = ScalarPerplex (x ^ r)
  {-# INLINE (^) #-}
instance (Absolute x x) => Absolute (Scalar (Perplex x)) x where
  absolute :: Scalar (Perplex x) -> x
  absolute (ScalarPerplex k) = absolute k
  {-# INLINE absolute #-}
instance (Absolute x x) => Absolute (Scalar (Perplex x)) (Scalar (Perplex x)) where
  absolute :: Scalar (Perplex x) -> Scalar (Perplex x)
  absolute (ScalarPerplex k) = ScalarPerplex (absolute k)
  {-# INLINE absolute #-}

instance (Semiring x) => Semiring (Scalar (Perplex x))
instance (Ring x) => Ring (Scalar (Perplex x))

instance (Distributive x) => Distributive (Perplex x)
instance (Semiring x) => Semiring (Perplex x)
instance (Ring x) => Ring (Perplex x)
instance (Ring x) => Module (Perplex x) where
  newtype Scalar (Perplex x) = ScalarPerplex {unScalar :: x}
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
instance (Field x) => Vector (Perplex x)
instance (Ring x) => Bilinear (Perplex x) where
  (•) :: Perplex x -> Perplex x -> Scalar (Perplex x)
  (x :! y) • (u :! v) = ScalarPerplex (x * u - y * v)
  {-# INLINE (•) #-}
instance (Ring x) => Algebra (Perplex x)
instance (Ring x) => Unital (Perplex x)
instance (Ring x) => AssociativeAlgebra (Perplex x)
instance (MultiplicativeAbelian x, Ring x) => CommutativeAlgebra (Perplex x)
