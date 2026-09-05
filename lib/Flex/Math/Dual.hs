module Flex.Math.Dual
  ( Dual ((:&))
  , DualBasis (Primal, Dual)
  , epsilon
  , apply
  , primal
  , tangent
  , derivative
  , applied
  , Scalar (..)
  ) where

import Flex.Math.Algebra
import Flex.Math.Category
import Flex.Math.Module
import Flex.Math.Numbers

import Data.Bool
import Data.Bounded (Bounded)
import Data.Either
import Data.Enum (Enum (..))
import Data.Eq (Eq (..))
import Data.Foldable qualified as Data
import Data.Foldable1 qualified as Data
import Data.Functor qualified as Data
import Data.Ord (Ord)
import Data.Traversable qualified as Data
import Data.Tuple
import GHC.Generics (Generic)
import Text.Read (Read)
import Text.Show (Show)

infixl 4 :&
data Dual x = x :& x
  deriving
    ( Eq
    , Ord
    , Show
    , Read
    , Data.Functor
    , Data.Foldable
    , Data.Traversable
    )

data DualBasis
  = Primal
  | Dual
  deriving (Eq, Ord, Enum, Bounded)

instance Morphisms (->) (->) Dual where
  morphism :: (x -> y) -> Dual x -> Dual y
  morphism = Data.fmap
instance Folds (->) (->) Dual where
  foldWith :: (Monoid z) => (x -> z) -> Dual x -> z
  foldWith x_z (xp :& xt) = x_z xp <> x_z xt
instance Folds1 (->) (->) Dual where
  foldWith1 :: (Semigroup z) => (x -> z) -> Dual x -> z
  foldWith1 x_z (xp :& xt) = x_z xp <> x_z xt
instance Traversals (->) (->) Dual where
  traverse :: (Applicative g) => (x -> g y) -> Dual x -> g (Dual y)
  traverse x_gy (xp :& xt) = liftA2 (:&) (x_gy xp) (x_gy xt)
instance Traversals1 (->) (->) Dual where
  traverse1 :: (Apply g) => (x -> g y) -> Dual x -> g (Dual y)
  traverse1 x_gy (xp :& xt) = liftA2 (:&) (x_gy xp) (x_gy xt)
instance Morphisms (Ix (Either () ())) (->) Dual where
  morphism :: Ix (Either () ()) x y -> Dual x -> Dual y
  morphism (Ix e_x_y) (xp :& xt) =
    e_x_y (Left ()) xp :& e_x_y (Right ()) xt
instance Folds (Ix (Either () ())) (->) Dual where
  foldWith :: (Monoid z) => Ix (Either () ()) x z -> Dual x -> z
  foldWith (Ix e_x_z) (xp :& xt) = e_x_z (Left ()) xp <> e_x_z (Right ()) xt
instance Traversals (Ix (Either () ())) (->) Dual where
  traverse :: (Applicative g) => Ix (Either () ()) x (g y) -> Dual x -> g (Dual y)
  traverse (Ix e_x_gy) (xp :& xt) = liftA2 (:&) (e_x_gy (Left ()) xp) (e_x_gy (Right ()) xt)
instance Folds1 (Ix (Either () ())) (->) Dual where
  foldWith1 :: (Semigroup z) => Ix (Either () ()) x z -> Dual x -> z
  foldWith1 (Ix e_x_z) (xp :& xt) = e_x_z (Left ()) xp <> e_x_z (Right ()) xt
instance Traversals1 (Ix (Either () ())) (->) Dual where
  traverse1 :: (Apply g) => Ix (Either () ()) x (g y) -> Dual x -> g (Dual y)
  traverse1 (Ix e_x_gy) (xp :& xt) = liftA2 (:&) (e_x_gy (Left ()) xp) (e_x_gy (Right ()) xt)
instance Apply Dual where
  (<*>) :: Dual (x -> y) -> Dual x -> Dual y
  (py :& ty) <*> (xp :& xt) = py xp :& ty xt
instance Collectable Dual where
  distribute :: (Along f) => f (Dual x) -> Dual (f x)
  distribute fd = morphism primal fd :& morphism tangent fd
instance Tabulation Dual where
  type Table Dual = DualBasis
  fromTable :: (DualBasis -> a) -> Dual a
  fromTable f = f Primal :& f Dual
  toTable :: Dual a -> DualBasis -> a
  toTable (r :& i) = \case
    Primal -> r
    Dual -> i
instance Data.Foldable1 Dual where
  foldMap1 :: (Semigroup m) => (x -> m) -> Dual x -> m
  foldMap1 x_m (xp :& xt) = x_m xp <> x_m xt

epsilon :: (Additive x, Multiplicative x) => Dual x
epsilon = zero :& one

apply :: (Multiplicative x) => (Dual x -> y) -> x -> y
apply f = f . (:& one)

primal :: Dual x -> x
primal (x :& _) = x

tangent :: Dual x -> x
tangent (_ :& x) = x

derivative :: (Multiplicative x) => (Dual x -> Dual x) -> x -> x
derivative f x = tangent (apply f x)

applied ::
  (Additive x, Multiplicative x, Data.Traversable xt) =>
  (xt (Dual x) -> y) -> xt x -> xt y
applied f xs = snd (Data.mapAccumL outer (zero @Int) xs)
 where
  outer !i _ = (succ i, f (snd (Data.mapAccumL (innr i) (zero @Int) xs)))
  innr !i !j x = (succ j, if i == j then x :& one else x :& zero)

instance (Additive y, From y x) => From y (Dual x) where
  from :: y -> Dual x
  from y = from y :& from @y zero

instance (Addition x x x) => Addition (Dual x) (Dual x) (Dual x) where
  (+.) :: Dual x -> Dual x -> Dual x
  (x :& x') +. (y :& y') = (x + y) :& (x' + y')
instance (Additive x) => Additive (Dual x) where
  zero :: Dual x
  zero = zero :& zero
instance (AdditiveAbelian x) => AdditiveAbelian (Dual x)
instance (Subtraction x x x) => Subtraction (Dual x) (Dual x) (Dual x) where
  (-.) :: Dual x -> Dual x -> Dual x
  (x :& x') -. (y :& y') = (x - y) :& (x' - y')
instance (AdditiveGroup x) => AdditiveGroup (Dual x) where
  negative :: (AdditiveGroup x) => Dual x -> Dual x
  negative (x :& x') = negative x :& negative x'

instance
  (Addition x x x, Multiplication x x x) =>
  Multiplication (Dual x) (Dual x) (Dual x)
  where
  (*.) :: Dual x -> Dual x -> Dual x
  (x :& x') *. (y :& y') = (x * y) :& (x * y' + x' * y)
instance (Additive x, Multiplicative x) => Multiplicative (Dual x) where
  one :: Dual x
  one = one :& zero
instance (Additive x, MultiplicativeAbelian x) => MultiplicativeAbelian (Dual x)
instance
  (Addition x x x, Multiplication x x x, Division x x x) =>
  Division (Dual x) (Dual x) (Dual x)
  where
  (/.) :: Dual x -> Dual x -> Dual x
  (x :& x') /. (y :& y') = (x / y) :& ((x' / y) + ((x * y') / (y * y)))
instance (AdditiveGroup x, MultiplicativeGroup x) => MultiplicativeGroup (Dual x) where
  reciprocal :: Dual x -> Dual x
  reciprocal (x :& x') = reciprocal x :& negative x' * reciprocal (x * x)

instance (Multiplication x x x) => Multiplication x (Dual x) (Dual x) where
  (*.) :: x -> Dual x -> Dual x
  k *. (x :& x') = k * x :& k * x'
instance (Multiplication x x x) => Multiplication (Dual x) x (Dual x) where
  (*.) :: Dual x -> x -> Dual x
  (x :& x') *. k = x * k :& x' * k
instance (Division x x x) => Division (Dual x) x (Dual x) where
  (/.) :: Dual x -> x -> Dual x
  (x :& x') /. k = x / k :& x' / k

instance (From y x) => From y (Scalar (Dual x)) where
  from :: y -> Scalar (Dual x)
  from n = ScalarDual (from n)

instance
  (Addition x x x) =>
  Addition (Scalar (Dual x)) (Scalar (Dual x)) (Scalar (Dual x))
  where
  (+.) :: Scalar (Dual x) -> Scalar (Dual x) -> Scalar (Dual x)
  ScalarDual x +. ScalarDual y = ScalarDual (x + y)
instance
  (Subtraction x x x) =>
  Subtraction (Scalar (Dual x)) (Scalar (Dual x)) (Scalar (Dual x))
  where
  (-.) :: Scalar (Dual x) -> Scalar (Dual x) -> Scalar (Dual x)
  ScalarDual x -. ScalarDual y = ScalarDual (x - y)
instance
  (Multiplication x x x) =>
  Multiplication (Scalar (Dual x)) (Scalar (Dual x)) (Scalar (Dual x))
  where
  (*.) :: Scalar (Dual x) -> Scalar (Dual x) -> Scalar (Dual x)
  ScalarDual x *. ScalarDual y = ScalarDual (x * y)
instance
  (Division x x x) =>
  Division (Scalar (Dual x)) (Scalar (Dual x)) (Scalar (Dual x))
  where
  (/.) :: Scalar (Dual x) -> Scalar (Dual x) -> Scalar (Dual x)
  ScalarDual x /. ScalarDual y = ScalarDual (x / y)
instance (Multiplication x x x) => Multiplication (Scalar (Dual x)) (Dual x) (Dual x) where
  (*.) :: Scalar (Dual x) -> Dual x -> Dual x
  ScalarDual k *. (x :& x') = k * x :& k * x'
instance (Multiplication x x x) => Multiplication (Dual x) (Scalar (Dual x)) (Dual x) where
  (*.) :: Dual x -> Scalar (Dual x) -> Dual x
  (x :& x') *. ScalarDual k = x * k :& x' * k
instance (Division x x x) => Division (Dual x) (Scalar (Dual x)) (Dual x) where
  (/.) :: Dual x -> Scalar (Dual x) -> Dual x
  (x :& x') /. ScalarDual k = x / k :& x' / k
instance (Power x r x) => Power (Scalar (Dual x)) r (Scalar (Dual x)) where
  (^) :: Scalar (Dual x) -> r -> Scalar (Dual x)
  ScalarDual x ^ r = ScalarDual (x ^ r)
instance (Absolute x x) => Absolute (Scalar (Dual x)) x where
  absolute :: Scalar (Dual x) -> x
  absolute (ScalarDual k) = absolute k
instance (Absolute x x) => Absolute (Scalar (Dual x)) (Scalar (Dual x)) where
  absolute :: Scalar (Dual x) -> Scalar (Dual x)
  absolute (ScalarDual k) = ScalarDual (absolute k)

instance (Semiring x) => Semiring (Scalar (Dual x))
instance (Ring x) => Ring (Scalar (Dual x))

instance (Distributive x) => Distributive (Dual x)
instance (Semiring x) => Semiring (Dual x)
instance (Ring x) => Ring (Dual x)
instance (Ring x) => Module (Dual x) where
  newtype Scalar (Dual x) = ScalarDual {unScalar :: x}
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
instance (Field x) => Vector (Dual x)
instance (Ring x) => Algebra (Dual x)
instance (Ring x) => Unital (Dual x)
instance (Ring x) => AssociativeAlgebra (Dual x)
instance (MultiplicativeAbelian x, Ring x) => CommutativeAlgebra (Dual x)
instance (MultiplicativeGroup x, Ring x) => DivisionAlgebra (Dual x)

instance
  ( Eq x
  , Additive x
  , Multiplicative x
  , From Natural x
  , Power x Natural x
  ) =>
  Power (Dual x) Natural (Dual x)
  where
  (^) :: Dual x -> Natural -> Dual x
  (x :& x') ^ k
    | k == zero = one :& zero
    | otherwise = x ^ k :& x' * from k * (x ^ pred k)

instance (AdditiveGroup x, Absolute x x, Signed x) => Absolute (Dual x) (Dual x) where
  absolute :: Dual x -> Dual x
  absolute (x :& x') =
    absolute x :& case sign x of
      Negative -> negative x'
      Unsigned -> zero
      Positive -> x'

instance (MultiplicativeGroup x, Logarithmic x) => Logarithmic (Dual x) where
  exp :: Dual x -> Dual x
  exp (x :& x') = exp x :& x' * exp x
  log :: Dual x -> Dual x
  log (x :& x') = log x :& x' / x
  logBase :: Dual x -> Dual x -> Dual x
  logBase (b :& _) (x :& x') = logBase b x :& x' / (x * log b)

instance (Root x, Trigonometric x) => Trigonometric (Dual x) where
  pi :: Dual x
  pi = pi :& zero
  sin :: Dual x -> Dual x
  sin (x :& x') = sin x :& x' * cos x
  cos :: Dual x -> Dual x
  cos (x :& x') = cos x :& negative x' * sin x
  tan :: Dual x -> Dual x
  tan (x :& x') = tan x :& x' / (cos x * cos x)
  arcsin :: Dual x -> Dual x
  arcsin (x :& x') = arcsin x :& x' / (2 √ (one - (x * x)))
  arccos :: Dual x -> Dual x
  arccos (x :& x') = arccos x :& negative x' / (2 √ (one - (x * x)))
  arctan :: Dual x -> Dual x
  arctan (x :& x') = arctan x :& x' / (one + x * x)

instance (Additive x) => Additive (Quaternion (Dual x)) where
  zero :: Quaternion (Dual x)
  zero = Quaternion zero zero zero zero
instance (AdditiveGroup x, Multiplicative x) => Multiplicative (Quaternion (Dual x)) where
  one :: Quaternion (Dual x)
  one = Quaternion one zero zero zero
