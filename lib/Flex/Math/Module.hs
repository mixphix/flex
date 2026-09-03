{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Flex.Math.Module
  ( Module (Scalar)
  , Scalar (..)
  , Vector
  , Bilinear ((•))
  , qd
  , Sesquilinear ((<•>))
  , quadrance
  , quadrature
  , normalized
  , InnerProduct
  , Complex ((:+))
  , eye
  , real
  , imag
  , Quaternion (..)
  , QuaternionBasis (E, I, J, K)
  , variable
  , Signature (..)
  , Term (..)
  , Laws (..)
  ) where

import Flex.Math.Category
import Flex.Math.Numbers
import Flex.Math.Structure

import Data.Bool (Bool, (||))
import Data.Complex (Complex ((:+)))
import Data.Eq (Eq (..))
import Data.Foldable qualified as Data
import Data.Functor qualified as Data
import Data.Kind (Type)
import Data.List1
import Data.Monoid (Monoid)
import Data.Ord (Ord (..))
import Data.Semigroup (Semigroup ((<>)))
import Data.Traversable qualified as Data
import GHC.Generics (Generic)
import GHC.Show (Show)

-- Complex numbers

eye :: (Additive x, Multiplicative x) => Complex x
eye = zero :+ one

real :: Complex x -> x
real (r :+ _) = r

imag :: Complex x -> x
imag (_ :+ i) = i

instance (From y x) => From y (Scalar (Complex x)) where
  from :: y -> Scalar (Complex x)
  from n = ScalarComplex (from n)

instance
  (Addition x x x) =>
  Addition
    (Scalar (Complex x))
    (Scalar (Complex x))
    (Scalar (Complex x))
  where
  (+.) :: Scalar (Complex x) -> Scalar (Complex x) -> Scalar (Complex x)
  ScalarComplex x +. ScalarComplex y = ScalarComplex (x + y)
instance
  (Subtraction x x x) =>
  Subtraction
    (Scalar (Complex x))
    (Scalar (Complex x))
    (Scalar (Complex x))
  where
  (-.) :: Scalar (Complex x) -> Scalar (Complex x) -> Scalar (Complex x)
  ScalarComplex x -. ScalarComplex y = ScalarComplex (x - y)
instance
  (Multiplication x x x) =>
  Multiplication
    (Scalar (Complex x))
    (Scalar (Complex x))
    (Scalar (Complex x))
  where
  (*.) :: Scalar (Complex x) -> Scalar (Complex x) -> Scalar (Complex x)
  ScalarComplex x *. ScalarComplex y = ScalarComplex (x * y)
instance
  (Division x x x) =>
  Division
    (Scalar (Complex x))
    (Scalar (Complex x))
    (Scalar (Complex x))
  where
  (/.) :: Scalar (Complex x) -> Scalar (Complex x) -> Scalar (Complex x)
  ScalarComplex x /. ScalarComplex y = ScalarComplex (x / y)
instance
  (Multiplication x x x) =>
  Multiplication
    (Scalar (Complex x))
    (Complex x)
    (Complex x)
  where
  (*.) :: Scalar (Complex x) -> Complex x -> Complex x
  ScalarComplex k *. x = k *. x
instance
  (Multiplication x x x) =>
  Multiplication
    (Complex x)
    (Scalar (Complex x))
    (Complex x)
  where
  (*.) :: Complex x -> Scalar (Complex x) -> Complex x
  x *. ScalarComplex k = x *. k
instance (Power x r x) => Power (Scalar (Complex x)) r (Scalar (Complex x)) where
  (^) :: Scalar (Complex x) -> r -> Scalar (Complex x)
  ScalarComplex x ^ r = ScalarComplex (x ^ r)

instance (Semiring x) => Semiring (Scalar (Complex x))
instance (Ring x) => Ring (Scalar (Complex x))
instance (Domain x) => Domain (Scalar (Complex x))

-- Module

class
  ( Ring (Scalar v)
  , AdditiveGroup v
  , AdditiveAbelian v
  , Multiplication (Scalar v) v v
  , Multiplication v (Scalar v) v
  ) =>
  Module v
  where
  data Scalar v :: Type
instance Structure Module where
  data Signature Module v
    = ModuleScalarRing (Signature Ring (Scalar v))
    | ModuleAdditiveGroup (Signature AdditiveGroup v)
    | ModuleScale (Scalar v) v
    deriving (Generic)
  data Term Module v
    = TermModule v
    | TermModuleScalar (Scalar v)
  operations :: (Module v) => Signature Module v -> Term Module v
  operations = \case
    ModuleScalarRing sig -> case operations sig of
      TermRing op -> TermModuleScalar op
    ModuleAdditiveGroup sig -> case operations sig of
      TermAdditiveGroup op -> TermModule op
    ModuleScale k v -> TermModule (k *. v)
  type Requirements Module = C2 Eq (CC Eq Scalar)
  data Laws Module v
    = ModuleRingLaws (Laws Ring (Scalar v))
    | ModuleAdditiveGroupLaws (Laws AdditiveAbelian v)
    | ModuleAdditiveAbelianLaws (Laws AdditiveAbelian v)
    | ModuleScaleOneIdentityLeft v
    | ModuleScaleOneIdentityRight v
    | ModuleScaleLeftDistributive (Scalar v) v v
    | ModuleScaleRightDistributive (Scalar v) v v
    | ModuleAddLeftDistributive (Scalar v) (Scalar v) v
    | ModuleAddRightDistributive (Scalar v) (Scalar v) v
    | ModuleMulLeftDistributive (Scalar v) (Scalar v) v
    | ModuleMulRightDistributive (Scalar v) (Scalar v) v
    deriving (Generic)
  lawful ::
    forall v.
    (Module v, Requirements Module v) =>
    Laws Module v -> Bool
  lawful = \case
    ModuleRingLaws laws -> lawful laws
    ModuleAdditiveGroupLaws laws -> lawful laws
    ModuleAdditiveAbelianLaws laws -> lawful laws
    ModuleScaleOneIdentityLeft v -> one @(Scalar v) *. v == v
    ModuleScaleOneIdentityRight v -> v *. one @(Scalar v) == v
    ModuleScaleLeftDistributive k v w ->
      let (+*) = (*.) @(Scalar v) @v @v
       in k +* (v + w) == k +* v + k +* w
    ModuleScaleRightDistributive k v w ->
      let (*+) = (*.) @v @(Scalar v) @v
       in (v + w) *+ k == v *+ k + w *+ k
    ModuleAddLeftDistributive a b v ->
      let (+*) = (*.) @(Scalar v) @v @v
       in (a + b) +* v == a +* v + b +* v
    ModuleAddRightDistributive a b v ->
      let (*+) = (*.) @v @(Scalar v) @v
       in v *+ (a + b) == v *+ a + v *+ b
    ModuleMulLeftDistributive a b v ->
      let (+*) = (*.) @(Scalar v) @v @v
       in (a * b) +* v == a +* (b +* v)
    ModuleMulRightDistributive a b v ->
      let (*+) = (*.) @v @(Scalar v) @v
       in v *+ (b * a) == (v *+ b) *+ a
deriving instance (Show v, Show (Scalar v)) => Show (Signature Module v)
deriving instance (Show v, Show (Scalar v)) => Show (Laws Module v)

instance (Ring x) => Module (Complex x) where
  newtype Scalar (Complex x) = ScalarComplex {unScalar :: x}
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
instance (Absolute x x) => Absolute (Scalar (Complex x)) x where
  absolute :: Scalar (Complex x) -> x
  absolute (ScalarComplex k) = absolute k
instance (Absolute x x) => Absolute (Scalar (Complex x)) (Scalar (Complex x)) where
  absolute :: Scalar (Complex x) -> Scalar (Complex x)
  absolute (ScalarComplex k) = ScalarComplex (absolute k)

-- Vector

class (Module v, Field (Scalar v)) => Vector v
instance Structure Vector where
  data Signature Vector v
    = VectorModule (Signature Module v)
    deriving (Generic)
  data Term Vector v
    = TermVector v
    | TermVectorScalar (Scalar v)
  operations :: (Vector v) => Signature Vector v -> Term Vector v
  operations = \case
    VectorModule sig -> case operations sig of
      TermModule op -> TermVector op
      TermModuleScalar op -> TermVectorScalar op
  type Requirements Vector = C2 Eq (CC Eq Scalar)
  data Laws Vector v
    = VectorModuleLaws (Laws Module v)
    | VectorFieldLaws (Laws Field (Scalar v))
    deriving (Generic)
  lawful :: (Vector v, Requirements Vector v) => Laws Vector v -> Bool
  lawful = \case
    VectorModuleLaws laws -> lawful laws
    VectorFieldLaws laws -> lawful laws
deriving instance (Show v, Show (Scalar v)) => Show (Signature Vector v)
deriving instance (Show v, Show (Scalar v)) => Show (Laws Vector v)

instance (Field x) => Vector (Complex x)

-- Bilinear

infixl 5 •
class (Module v) => Bilinear v where
  (•) :: v -> v -> Scalar v

qd :: (Bilinear v) => v -> Scalar v
qd = join (•)

instance Structure Bilinear where
  data Signature Bilinear v
    = BilinearModule (Signature Module v)
    | BilinearDot v v
    | BilinearQd v
    deriving (Generic)
  data Term Bilinear v
    = TermBilinear v
    | TermBilinearScalar (Scalar v)
  operations ::
    (Bilinear v) =>
    Signature Bilinear v ->
    Term Bilinear v
  operations = \case
    BilinearModule sig -> case operations sig of
      TermModule op -> TermBilinear op
      TermModuleScalar op -> TermBilinearScalar op
    BilinearDot u0 u1 -> TermBilinearScalar (u0 • u1)
    BilinearQd u -> TermBilinearScalar (qd u)
  type Requirements Bilinear = C2 Eq (CC Eq Scalar)
  data Laws Bilinear v
    = BilinearModuleLaws (Laws Module v)
    | BilinearLinearFirst (Scalar v) v (Scalar v) v v
    | BilinearLinearSecond v (Scalar v) v (Scalar v) v
    deriving (Generic)
  lawful ::
    forall v.
    (Bilinear v, Requirements Bilinear v) =>
    Laws Bilinear v -> Bool
  lawful = \case
    BilinearModuleLaws laws -> lawful laws
    BilinearLinearFirst a x b y z ->
      (a *. x + b *. y) • z == a * (x • z) + b * (y • z)
    BilinearLinearSecond x a y b z ->
      x • (a *. y + b *. z) == a * (x • y) + b * (x • z)
deriving instance (Show v, Show (Scalar v)) => Show (Signature Bilinear v)
deriving instance (Show v, Show (Scalar v)) => Show (Laws Bilinear v)

-- Sesquilinear

infixl 5 <•>
class (Module v, Conjugate (Scalar v)) => Sesquilinear v where
  (<•>) :: v -> v -> Scalar v

quadrance :: (Sesquilinear v) => v -> Scalar v
quadrance v = v <•> v

quadrature :: (Sesquilinear v) => v -> v -> Scalar v
quadrature u v = quadrance (u - v)

normalized ::
  (Sesquilinear v, Root (Scalar v), Division v (Scalar v) v) => v -> v
normalized u = u /. (2 √ quadrance u)

instance Structure Sesquilinear where
  data Signature Sesquilinear v
    = SesquilinearModule (Signature Module v)
    | SesquilinearConjugateScalar (Scalar v)
    | SesquilinearAngleDot v v
    | SesquilinearQuadrance v
    deriving (Generic)
  data Term Sesquilinear v
    = TermSesquilinear v
    | TermSesquilinearScalar (Scalar v)
  operations ::
    (Sesquilinear x) =>
    Signature Sesquilinear x ->
    Term Sesquilinear x
  operations = \case
    SesquilinearModule sig -> case operations sig of
      TermModule op -> TermSesquilinear op
      TermModuleScalar op -> TermSesquilinearScalar op
    SesquilinearConjugateScalar x -> TermSesquilinearScalar (conjugate x)
    SesquilinearAngleDot u0 u1 -> TermSesquilinearScalar (u0 <•> u1)
    SesquilinearQuadrance u -> TermSesquilinearScalar (quadrance u)
  type Requirements Sesquilinear = C2 Eq (CC Eq Scalar)
  data Laws Sesquilinear v
    = SesquilinearConjugateSwap v v
    | SesquilinearLinearFirst (Scalar v) v (Scalar v) v v
    | SesquilinearConjugateLinearSecond v (Scalar v) v (Scalar v) v
    deriving (Generic)
  lawful ::
    (Sesquilinear x, Requirements Sesquilinear x) =>
    Laws Sesquilinear x -> Bool
  lawful = \case
    SesquilinearConjugateSwap v w -> v <•> w == conjugate (w <•> v)
    SesquilinearLinearFirst a x b y z ->
      (a *. x + b *. y)
        <•> z
        == a
        * (x <•> z)
        + b
        * (y <•> z)
    SesquilinearConjugateLinearSecond x a y b z ->
      x
        <•> (a *. y + b *. z)
        == (conjugate a * (x <•> y))
        + (conjugate b * (x <•> z))
deriving instance (Show v, Show (Scalar v)) => Show (Signature Sesquilinear v)
deriving instance (Show v, Show (Scalar v)) => Show (Laws Sesquilinear v)

instance (Ring x, Conjugate x) => Sesquilinear (Complex x) where
  (<•>) :: Complex x -> Complex x -> Scalar (Complex x)
  r1 :+ i1 <•> r2 :+ i2 = ScalarComplex ((r1 * r2) + (i1 * i2))

-- InnerProduct

class (Sesquilinear v) => InnerProduct v
instance Structure InnerProduct where
  data Signature InnerProduct v
    = InnerProductSesquilinear (Signature Sesquilinear v)
    deriving (Generic)
  data Term InnerProduct v
    = TermInnerProduct v
    | TermInnerProductScalar (Scalar v)
  operations ::
    (InnerProduct v) =>
    Signature InnerProduct v ->
    Term InnerProduct v
  operations = \case
    InnerProductSesquilinear sig -> case operations sig of
      TermSesquilinear op -> TermInnerProduct op
      TermSesquilinearScalar x -> TermInnerProductScalar x
  type Requirements InnerProduct = C2 Eq (CC Eq Scalar)
  data Laws InnerProduct v
    = InnerProductSesquilinearLaws (Laws Sesquilinear v)
    | InnerProductPositiveDefinite v
    deriving (Generic)
  lawful ::
    forall v.
    (InnerProduct v, Requirements InnerProduct v) =>
    Laws InnerProduct v -> Bool
  lawful = \case
    InnerProductSesquilinearLaws laws -> lawful laws
    InnerProductPositiveDefinite v -> v <•> v /= zero || v == zero
deriving instance (Show v, Show (Scalar v)) => Show (Signature InnerProduct v)
deriving instance (Show v, Show (Scalar v)) => Show (Laws InnerProduct v)

instance (Ring x, Conjugate x) => InnerProduct (Complex x)

-- Quaternions

data Quaternion x = Quaternion {e :: !x, i :: !x, j :: !x, k :: !x}
  deriving
    ( Eq
    , Ord
    , Show
    , Data.Functor
    , Data.Foldable
    , Data.Traversable
    , Generic
    )

instance Morphisms (->) (->) Quaternion where
  morphism :: (x -> y) -> Quaternion x -> Quaternion y
  morphism = Data.fmap
instance Folds (->) (->) Quaternion where
  foldWith :: (Monoid z) => (x -> z) -> Quaternion x -> z
  foldWith x_z (Quaternion e i j k) = x_z e <> x_z i <> x_z j <> x_z k
instance Traversals (->) (->) Quaternion where
  traverse :: (Applicative g) => (x -> g y) -> Quaternion x -> g (Quaternion y)
  traverse x_gy (Quaternion e i j k) =
    liftA3 Quaternion (x_gy e) (x_gy i) (x_gy j) <*> (x_gy k)
instance Collectable Quaternion where
  distribute :: (Along f) => f (Quaternion x) -> Quaternion (f x)
  distribute f = Quaternion
    do morphism (.e) f
    do morphism (.i) f
    do morphism (.j) f
    do morphism (.k) f

data QuaternionBasis
  = E
  | I
  | J
  | K
instance Tabulation Quaternion where
  type Table Quaternion = QuaternionBasis
  fromTable :: (Table Quaternion -> x) -> Quaternion x
  fromTable f = Quaternion (f E) (f I) (f J) (f K)
  toTable :: Quaternion x -> Table Quaternion -> x
  toTable (Quaternion e i j k) = \case
    E -> e
    I -> i
    J -> j
    K -> k

instance (Additive y, From y x) => From y (Quaternion x) where
  from :: y -> Quaternion x
  from n =
    Quaternion
      (from n)
      (from @y zero)
      (from @y zero)
      (from @y zero)

instance
  (Addition x x x) =>
  Addition (Quaternion x) (Quaternion x) (Quaternion x)
  where
  (+.) :: Quaternion x -> Quaternion x -> Quaternion x
  Quaternion z1 i1 j1 k1 +. Quaternion z2 i2 j2 k2 =
    Quaternion (z1 + z2) (i1 + i2) (j1 + j2) (k1 + k2)
instance (Additive x) => Additive (Quaternion x) where
  zero :: Quaternion x
  zero = Quaternion zero zero zero zero
instance (AdditiveAbelian x) => AdditiveAbelian (Quaternion x)

instance
  (Subtraction x x x) =>
  Subtraction (Quaternion x) (Quaternion x) (Quaternion x)
  where
  (-.) :: Quaternion x -> Quaternion x -> Quaternion x
  Quaternion z1 i1 j1 k1 -. Quaternion z2 i2 j2 k2 =
    Quaternion (z1 - z2) (i1 - i2) (j1 - j2) (k1 - k2)
instance (AdditiveGroup x) => AdditiveGroup (Quaternion x) where
  negative :: Quaternion x -> Quaternion x
  negative (Quaternion z i j k) =
    Quaternion (negative z) (negative i) (negative j) (negative k)

instance
  (Addition x x x, Subtraction x x x, Multiplication x x x) =>
  Multiplication (Quaternion x) (Quaternion x) (Quaternion x)
  where
  (*.) :: Quaternion x -> Quaternion x -> Quaternion x
  Quaternion a1 b1 c1 d1 *. Quaternion a2 b2 c2 d2 =
    Quaternion
      do (a1 * a2) - (b1 * b2) - (c1 * c2) - (d1 * d2)
      do (a1 * b2) + (b1 * a2) + (c1 * d2) - (d1 * c2)
      do (a1 * c2) - (b1 * d2) + (c1 * a2) + (d1 * b2)
      do (a1 * d2) + (b1 * c2) - (c1 * b2) + (d1 * a2)
instance
  (Additive x, Subtraction x x x, Multiplicative x) =>
  Multiplicative (Quaternion x)
  where
  one :: Quaternion x
  one = Quaternion one zero zero zero

instance
  (AdditiveGroup x, MultiplicativeGroup x, Conjugate x) =>
  Division (Quaternion x) (Quaternion x) (Quaternion x)
  where
  (/.) :: Quaternion x -> Quaternion x -> Quaternion x
  a /. b@(Quaternion bz bi bj bk) =
    reciprocal ((bz * bz) + (bi * bi) + (bj * bj) + (bk * bk))
      *. (a * conjugate b)

instance
  (AdditiveGroup x, MultiplicativeGroup x, Conjugate x) =>
  MultiplicativeGroup (Quaternion x)
  where
  reciprocal :: Quaternion x -> Quaternion x
  reciprocal b@(Quaternion bz bi bj bk) =
    reciprocal ((bz * bz) + (bi * bi) + (bj * bj) + (bk * bk))
      *. conjugate b

instance
  (Multiplication x x x) =>
  Multiplication x (Quaternion x) (Quaternion x)
  where
  (*.) :: x -> Quaternion x -> Quaternion x
  x *. Quaternion z i j k = Quaternion (x * z) (x * i) (x * j) (x * k)
instance
  (Multiplication x x x) =>
  Multiplication (Quaternion x) x (Quaternion x)
  where
  (*.) :: Quaternion x -> x -> Quaternion x
  Quaternion z i j k *. x = Quaternion (z * x) (i * x) (j * x) (k * x)

instance
  (Additive x, Subtraction x x x, Multiplicative x) =>
  Distributive (Quaternion x)
instance
  (Additive x, Subtraction x x x, Multiplicative x) =>
  Semiring (Quaternion x)
instance
  (AdditiveAbelian x, AdditiveGroup x, Multiplicative x) =>
  Ring (Quaternion x)

instance (From y x) => From y (Scalar (Quaternion x)) where
  from :: y -> Scalar (Quaternion x)
  from n = ScalarQuaternion (from n)

instance
  (Addition x x x) =>
  Addition
    (Scalar (Quaternion x))
    (Scalar (Quaternion x))
    (Scalar (Quaternion x))
  where
  (+.) ::
    Scalar (Quaternion x) ->
    Scalar (Quaternion x) ->
    Scalar (Quaternion x)
  ScalarQuaternion x +. ScalarQuaternion y = ScalarQuaternion (x + y)
instance
  (Subtraction x x x) =>
  Subtraction
    (Scalar (Quaternion x))
    (Scalar (Quaternion x))
    (Scalar (Quaternion x))
  where
  (-.) ::
    Scalar (Quaternion x) ->
    Scalar (Quaternion x) ->
    Scalar (Quaternion x)
  ScalarQuaternion x -. ScalarQuaternion y = ScalarQuaternion (x - y)
instance
  (Multiplication x x x) =>
  Multiplication
    (Scalar (Quaternion x))
    (Scalar (Quaternion x))
    (Scalar (Quaternion x))
  where
  (*.) ::
    Scalar (Quaternion x) ->
    Scalar (Quaternion x) ->
    Scalar (Quaternion x)
  ScalarQuaternion x *. ScalarQuaternion y = ScalarQuaternion (x * y)
instance
  (Division x x x) =>
  Division
    (Scalar (Quaternion x))
    (Scalar (Quaternion x))
    (Scalar (Quaternion x))
  where
  (/.) ::
    Scalar (Quaternion x) ->
    Scalar (Quaternion x) ->
    Scalar (Quaternion x)
  ScalarQuaternion x /. ScalarQuaternion y = ScalarQuaternion (x / y)
instance
  (Multiplication x x x) =>
  Multiplication
    (Scalar (Quaternion x))
    (Quaternion x)
    (Quaternion x)
  where
  (*.) ::
    Scalar (Quaternion x) ->
    Quaternion x ->
    Quaternion x
  ScalarQuaternion x *. e = x *. e
instance
  (Multiplication x x x) =>
  Multiplication
    (Quaternion x)
    (Scalar (Quaternion x))
    (Quaternion x)
  where
  (*.) ::
    Quaternion x ->
    Scalar (Quaternion x) ->
    Quaternion x
  e *. ScalarQuaternion x = e *. x
instance
  (Power x r x) =>
  Power (Scalar (Quaternion x)) r (Scalar (Quaternion x))
  where
  (^) :: Scalar (Quaternion x) -> r -> Scalar (Quaternion x)
  ScalarQuaternion x ^ r = ScalarQuaternion (x ^ r)
instance
  (Absolute x x) =>
  Absolute (Scalar (Quaternion x)) x
  where
  absolute :: Scalar (Quaternion x) -> x
  absolute (ScalarQuaternion k) = absolute k
instance
  (Absolute x x) =>
  Absolute (Scalar (Quaternion x)) (Scalar (Quaternion x))
  where
  absolute :: Scalar (Quaternion x) -> Scalar (Quaternion x)
  absolute (ScalarQuaternion k) = ScalarQuaternion (absolute k)

instance
  (Additive x, Subtraction x x x, Multiplicative x) =>
  Semiring (Scalar (Quaternion x))
instance
  (AdditiveAbelian x, AdditiveGroup x, Multiplicative x) =>
  Ring (Scalar (Quaternion x))
instance (Domain x) => Domain (Scalar (Quaternion x))

instance (Ring x) => Module (Quaternion x) where
  newtype Scalar (Quaternion x) = ScalarQuaternion {unScalar :: x}
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
instance (Field x) => Vector (Quaternion x)
instance (Ring x, Conjugate x) => Sesquilinear (Quaternion x) where
  (<•>) :: Quaternion x -> Quaternion x -> Scalar (Quaternion x)
  Quaternion z1 i1 j1 k1 <•> Quaternion z2 i2 j2 k2 =
    ScalarQuaternion ((z1 * z2) + (i1 * i2) + (j1 * j2) + (k1 * k2))
instance (Ring x, Conjugate x) => InnerProduct (Quaternion x)

instance (AdditiveGroup x) => Conjugate (Quaternion x) where
  conjugate :: Quaternion x -> Quaternion x
  conjugate (Quaternion z i j k) = Quaternion z (negative i) (negative j) (negative k)

-- Polynomials

variable :: (Additive x, Multiplicative x) => List1 x
variable = zero :|| Sole one

instance (From y x) => From y (Scalar (List1 x)) where
  from :: y -> Scalar (List1 x)
  from n = ScalarList1 (from n)

instance
  (Addition x x x) =>
  Addition (Scalar (List1 x)) (Scalar (List1 x)) (Scalar (List1 x))
  where
  (+.) :: Scalar (List1 x) -> Scalar (List1 x) -> Scalar (List1 x)
  ScalarList1 x +. ScalarList1 y = ScalarList1 (x + y)
instance
  (Subtraction x x x) =>
  Subtraction (Scalar (List1 x)) (Scalar (List1 x)) (Scalar (List1 x))
  where
  (-.) :: Scalar (List1 x) -> Scalar (List1 x) -> Scalar (List1 x)
  ScalarList1 x -. ScalarList1 y = ScalarList1 (x - y)
instance
  (Multiplication x x x) =>
  Multiplication (Scalar (List1 x)) (Scalar (List1 x)) (Scalar (List1 x))
  where
  (*.) :: Scalar (List1 x) -> Scalar (List1 x) -> Scalar (List1 x)
  ScalarList1 x *. ScalarList1 y = ScalarList1 (x * y)
instance
  (Division x x x) =>
  Division (Scalar (List1 x)) (Scalar (List1 x)) (Scalar (List1 x))
  where
  (/.) :: Scalar (List1 x) -> Scalar (List1 x) -> Scalar (List1 x)
  ScalarList1 x /. ScalarList1 y = ScalarList1 (x / y)
instance
  (Eq x, Additive x, Multiplicative x) =>
  Multiplication (Scalar (List1 x)) (List1 x) (List1 x)
  where
  (*.) :: Scalar (List1 x) -> List1 x -> List1 x
  ScalarList1 k *. x = k *. x
instance
  (Eq x, Additive x, Multiplicative x) =>
  Multiplication (List1 x) (Scalar (List1 x)) (List1 x)
  where
  (*.) :: List1 x -> Scalar (List1 x) -> List1 x
  x *. ScalarList1 k = x *. k
instance (Power x r x) => Power (Scalar (List1 x)) r (Scalar (List1 x)) where
  (^) :: Scalar (List1 x) -> r -> Scalar (List1 x)
  ScalarList1 x ^ r = ScalarList1 (x ^ r)
instance (Absolute x x) => Absolute (Scalar (List1 x)) x where
  absolute :: Scalar (List1 x) -> x
  absolute (ScalarList1 k) = absolute k
instance (Absolute x x) => Absolute (Scalar (List1 x)) (Scalar (List1 x)) where
  absolute :: Scalar (List1 x) -> Scalar (List1 x)
  absolute (ScalarList1 k) = ScalarList1 (absolute k)

instance
  (Additive x, Subtraction x x x, Multiplicative x) =>
  Semiring (Scalar (List1 x))
instance
  (AdditiveAbelian x, AdditiveGroup x, Multiplicative x) =>
  Ring (Scalar (List1 x))
instance (Domain x) => Domain (Scalar (List1 x))

instance (Ring x, AdditiveAbelian x, Eq x) => Module (List1 x) where
  newtype Scalar (List1 x) = ScalarList1 {unScalar :: x}
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
instance (Eq x, Field x) => Vector (List1 x)

instance (Additive x) => Additive (Quaternion (Complex x)) where
  zero :: Quaternion (Complex x)
  zero = Quaternion zero zero zero zero
instance (AdditiveGroup x, Multiplicative x) => Multiplicative (Quaternion (Complex x)) where
  one :: Quaternion (Complex x)
  one = Quaternion one zero zero zero
