{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Flex.Math.Matrix
  ( Matrix (transpose)
  , adjoint
  , Square (trace, determinant)
  , V (V, unV)
  , dimensions
  , (!)
  , push
  , pushBack
  , pop
  , popBack
  , toList
  , fromList
  , zipWith
  , zip
  , projection
  , householder
  , orthogonalize
  , orthonormalize
  , M (M, unM)
  , rows
  , unrows
  , row
  , columns
  , uncolumns
  , column
  , outer
  , toLists
  , qr
  , lu
  , system
  , hadamard
  , kronecker
  , minor
  , cofactor
  , cofactorMatrix
  , adjugate
  , characteristicPolynomial
  , diagonal
  , upperTriangular
  , lowerTriangular
  , symmetric
  , hermitian
  , vn
  , vnM
  , v1
  , withV1
  , v2
  , withV2
  , v3
  , withV3
  , v4
  , withV4
  , m22
  , m23
  , m24
  , m32
  , m33
  , m34
  , m42
  , m43
  , m44
  , Scalar (..)
  , Signature (..)
  , Term (..)
  , Laws (..)
  ) where

import Flex.Math.Algebra
import Flex.Math.Category
import Flex.Math.Foldable
import Flex.Math.Module
import Flex.Math.Numbers
import Flex.Math.Optics
import Flex.Math.Optics.TH
import Flex.Math.Structure

import Control.Applicative qualified as Control
import Control.Monad qualified as Control
import Data.Bool
import Data.Char (Char)
import Data.Enum
import Data.Eq
import Data.Finite (Finite, finite, getFinite)
import Data.Foldable qualified as Data
import Data.Function (const, flip, ($))
import Data.Functor qualified as Data
import Data.List qualified as List
import Data.List1 (List1, pattern Sole, pattern (:||))
import Data.Maybe
import Data.Monoid
import Data.Ord (Ord (..), Ordering (..))
import Data.Proxy
import Data.Traversable qualified as Data
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import GHC.Show
import GHC.TypeNats

class (Traversable m, Traversable n) => Matrix m n x where
  transpose :: m x -> n x

adjoint :: (Conjugate x, Matrix m n x) => m x -> n x
adjoint = morphism conjugate . transpose

class
  ( Matrix m m x
  , Eq (Scalar (m x))
  , Distributive (m x)
  , AssociativeAlgebra (m x)
  ) =>
  Square m x
  where
  trace :: m x -> Scalar (m x)
  determinant :: m x -> Scalar (m x)
instance Structure (Square m) where
  data Signature (Square m) x
    = SquareTrace (m x)
    | SquareDeterminant (m x)
    deriving (Generic)
  newtype Term (Square m) x = TermSquare (Scalar (m x))
  operations :: (Square m x) => Signature (Square m) x -> Term (Square m) x
  operations = TermSquare . \case
    SquareTrace a -> trace a
    SquareDeterminant a -> determinant a
  type Requirements (Square m) = CC Eq m
  data Laws (Square m) x
    = SquareAssociativeAlgebraLaws (Laws AssociativeAlgebra (m x))
    | SquareDistributiveLaws (Laws Distributive (m x))
    | SquareTraceTranspose (m x)
    | SquareTraceCyclic (m x) (m x) (m x)
    | SquareDeterminantProduct (m x) (m x)
    deriving (Generic)
  lawful ::
    forall x. (Square m x, Requirements (Square m) x) => Laws (Square m) x -> Bool
  lawful = \case
    SquareAssociativeAlgebraLaws laws -> lawful laws
    SquareDistributiveLaws laws -> lawful laws
    SquareTraceTranspose m -> trace m == trace (transpose @m @m m)
    SquareTraceCyclic a b c ->
      trace (a * (b * c)) == trace (b * (c * a))
        && trace (a * (b * c)) == trace (c * (a * b))
    SquareDeterminantProduct a b ->
      determinant (a * b) == determinant a * determinant b
deriving instance (Show (m x)) => Show (Signature (Square m) x)
deriving instance (Show (m x), Show (Scalar (m x))) => Show (Laws (Square m) x)

newtype V n x = V {unV :: Vector.Vector x} deriving (Data.Functor)

instance (KnownNat n, Eq x) => Eq (V n x) where
  (==) :: V n x -> V n x -> Bool
  V v == V w = v == w

instance (KnownNat n, Ord x) => Ord (V n x) where
  compare :: V n x -> V n x -> Ordering
  compare (V v) (V w) = fold (Vector.zipWith (\_v _w -> compare _v _w) v w)

instance (KnownNat n, Show x) => Show (V n x) where
  show :: V n x -> [Char]
  show v =
    "V {"
      <> foldWith (\i -> " " <> show (v ! i) <> " ") (dimensions (Proxy @n))
      <> "}"

dimensions :: forall n. (KnownNat n) => Proxy n -> [Natural]
dimensions Proxy = case natVal (Proxy @n) of
  0 -> []
  n -> [zero .. n - one @Natural]

(!) :: V n x -> Natural -> x
V f ! x = f Vector.! from x

push :: x -> V n x -> V (n + 1) x
push x (V v) = V (Vector.singleton x <> v)

pop :: V (n + 1) x -> (x, V n x)
pop vv@(V v) = (vv ! 0, V (Vector.drop 1 v))

pushBack :: forall n x. (KnownNat n) => x -> V n x -> V (n + 1) x
pushBack x (V v) = V (v <> Vector.singleton x)

popBack :: forall n x. (KnownNat n) => V (n + 1) x -> (x, V n x)
popBack (V v) = (Vector.last v, V (Vector.take (Vector.length v - 1) v))

setV :: Natural -> x -> V n x -> V n x
setV n x (V v) = V (v Vector.// [(from n, x)])

$(Data.traverse (instanceFieldV 0) [1 .. 4])
$(Data.traverse (instanceFieldV 1) [2 .. 4])
$(Data.traverse (instanceFieldV 2) [3 .. 4])
$(Data.traverse (instanceFieldV 3) [4])

instance Morphisms (->) (->) (V n) where
  morphism :: (x -> y) -> V n x -> V n y
  morphism = Data.fmap
instance (KnownNat n) => Folds (->) (->) (V n) where
  foldWith :: (Monoid m) => (x -> m) -> V n x -> m
  foldWith f (V v) = foldWith f v
instance (KnownNat n) => Traversals (->) (->) (V n) where
  traverse :: (Applicative f) => (a -> f b) -> V n a -> f (V n b)
  traverse f (V v) = morphism V (traverse f v)
instance Morphisms (Ix Natural) (->) (V n) where
  morphism :: Ix Natural x y -> V n x -> V n y
  morphism (Ix i_x_y) (V v) = V (morphism (Ix i_x_y) v)
instance (KnownNat n) => Folds (Ix Natural) (->) (V n) where
  foldWith :: (Monoid z) => Ix Natural x z -> V n x -> z
  foldWith (Ix i_x_z) (V v) = foldWith (Ix i_x_z) v
instance (KnownNat n) => Traversals (Ix Natural) (->) (V n) where
  traverse ::
    (Applicative f) => Ix Natural a (f b) -> V n a -> f (V n b)
  traverse (Ix f) (V v) = morphism V (traverse (Ix f) v)
instance (KnownNat n) => Data.Foldable (V n) where
  foldMap :: (Monoid m) => (x -> m) -> V n x -> m
  foldMap f (V v) = foldWith f v
instance (KnownNat n) => Data.Traversable (V n) where
  traverse :: (Control.Applicative f) => (a -> f b) -> V n a -> f (V n b)
  traverse f (V v) = V Data.<$> Data.traverse f v
instance (KnownNat n) => Pure (V n) where
  pure :: x -> V n x
  pure x = V (Vector.replicate (from (natVal (Proxy @n))) x)
instance (KnownNat n) => Apply (V n) where
  (<*>) :: V n (x -> y) -> V n x -> V n y
  V f <*> V x = V (f <*> x)
instance (KnownNat n) => Control.Applicative (V n) where
  pure :: x -> V n x
  pure x = V (Vector.replicate (from (natVal (Proxy @n))) x)
  (<*>) :: V n (x -> y) -> V n x -> V n y
  V f <*> V x = V (f Control.<*> x)

instance (KnownNat n) => Collectable (V n) where
  distribute :: (Along f) => f (V n x) -> V n (f x)
  distribute fv = vn (natVal (Proxy @n)) \i -> morphism (! i) fv
instance (KnownNat n) => Tabulation (V n) where
  type Table (V n) = Finite n
  fromTable :: (Table (V n) -> x) -> V n x
  fromTable tx = vn (natVal (Proxy @n)) \i -> tx (finite (from i))
  toTable :: V n x -> Table (V n) -> x
  toTable v i = v ! from (getFinite i)

instance (KnownNat n) => Each (V n x) (V n x) x x

toList :: forall n x. (KnownNat n) => V n x -> [x]
toList (V v) = Vector.toList v

fromList :: forall n x. (KnownNat n) => [x] -> Maybe (V n x)
fromList xs = case List.compareLength xs (from (natVal (Proxy @n))) of
  EQ -> Just (V (Vector.fromList xs))
  _ -> Nothing

instance (Addition x x x) => Addition (V n x) (V n x) (V n x) where
  (+.) :: V n x -> V n x -> V n x
  V f +. V g = V (Vector.zipWith (+.) f g)
instance (KnownNat n, Additive x) => Additive (V n x) where
  zero :: V n x
  zero = pure zero
instance (KnownNat n, AdditiveAbelian x) => AdditiveAbelian (V n x)

instance (Subtraction x x x) => Subtraction (V n x) (V n x) (V n x) where
  (-.) :: V n x -> V n x -> V n x
  V f -. V g = V (Vector.zipWith (-.) f g)

instance (KnownNat n, AdditiveGroup x) => AdditiveGroup (V n x) where
  negative :: V n x -> V n x
  negative (V v) = V (morphism negative v)

instance (Multiplication x x x) => Multiplication x (V n x) (V n x) where
  (*.) :: x -> V n x -> V n x
  x *. V v = V (morphism (x *.) v)
instance (Multiplication x x x) => Multiplication (V n x) x (V n x) where
  (*.) :: V n x -> x -> V n x
  V v *. x = V (morphism (*. x) v)
instance (Division x x x) => Division (V n x) x (V n x) where
  (/.) :: V n x -> x -> V n x
  V v /. x = V (morphism (/. x) v)

instance (KnownNat n, From y x) => From y (Scalar (V n x)) where
  from :: y -> Scalar (V n x)
  from n = ScalarV (from n)

instance (KnownNat n, Addition x x x) => Addition (Scalar (V n x)) (Scalar (V n x)) (Scalar (V n x)) where
  (+.) :: Scalar (V n x) -> Scalar (V n x) -> Scalar (V n x)
  ScalarV x +. ScalarV y = ScalarV (x + y)
instance (KnownNat n, Subtraction x x x) => Subtraction (Scalar (V n x)) (Scalar (V n x)) (Scalar (V n x)) where
  (-.) :: Scalar (V n x) -> Scalar (V n x) -> Scalar (V n x)
  ScalarV x -. ScalarV y = ScalarV (x - y)
instance (KnownNat n, Multiplication x x x) => Multiplication (Scalar (V n x)) (Scalar (V n x)) (Scalar (V n x)) where
  (*.) :: Scalar (V n x) -> Scalar (V n x) -> Scalar (V n x)
  ScalarV x *. ScalarV y = ScalarV (x * y)
instance (KnownNat n, Division x x x) => Division (Scalar (V n x)) (Scalar (V n x)) (Scalar (V n x)) where
  (/.) :: Scalar (V n x) -> Scalar (V n x) -> Scalar (V n x)
  ScalarV x /. ScalarV y = ScalarV (x / y)
instance (KnownNat n, Multiplication x x x) => Multiplication (Scalar (V n x)) (V n x) (V n x) where
  (*.) :: Scalar (V n x) -> V n x -> V n x
  ScalarV x *. v = x *. v
instance (KnownNat n, Multiplication x x x) => Multiplication (V n x) (Scalar (V n x)) (V n x) where
  (*.) :: V n x -> Scalar (V n x) -> V n x
  v *. ScalarV x = v *. x
instance (KnownNat n, Division x x x) => Division (V n x) (Scalar (V n x)) (V n x) where
  (/.) :: V n x -> Scalar (V n x) -> V n x
  v /. ScalarV x = v /. x
instance (KnownNat n, Power x Rational x) => Power (Scalar (V n x)) Rational (Scalar (V n x)) where
  (^) :: Scalar (V n x) -> Rational -> Scalar (V n x)
  ScalarV x ^ r = ScalarV (x ^ r)
instance (KnownNat n, Absolute x x) => Absolute (Scalar (V n x)) x where
  absolute :: Scalar (V n x) -> x
  absolute (ScalarV k) = absolute k
instance (KnownNat n, Absolute x x) => Absolute (Scalar (V n x)) (Scalar (V n x)) where
  absolute :: Scalar (V n x) -> Scalar (V n x)
  absolute (ScalarV k) = ScalarV (absolute k)

instance (KnownNat n, Semiring x) => Semiring (Scalar (V n x))
instance (KnownNat n, Ring x) => Ring (Scalar (V n x))

instance (KnownNat n, Ring x) => Module (V n x) where
  newtype Scalar (V n x) = ScalarV {unScalar :: x}
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
instance (KnownNat n, Field x) => Vector (V n x)
instance (KnownNat n, Ring x) => Bilinear (V n x) where
  (•) :: V n x -> V n x -> Scalar (V n x)
  V f • V g = ScalarV do
    sum (Vector.zipWith (*) f g)
instance (KnownNat n, Ring x, Conjugate x) => Sesquilinear (V n x) where
  (<•>) :: V n x -> V n x -> Scalar (V n x)
  V f <•> V g = ScalarV do
    sum (Vector.zipWith (\_f _g -> _f * conjugate _g) f g)
instance (KnownNat n, Ring x, Conjugate x) => InnerProduct (V n x)

zipWith ::
  forall n x y z. (x -> y -> z) -> V n x -> V n y -> V n z
zipWith f (V v) (V w) = V (Vector.zipWith f v w)

zip :: V n x -> V n y -> V n (x, y)
zip = zipWith (,)

projection ::
  forall n x. (KnownNat n, Field x, Conjugate x) => V n x -> V n x -> V n x
projection u v = ((u <•> v) / quadrance u) *. u

householder ::
  forall n x. (KnownNat n, Field x, Conjugate x) => V n x -> V n x -> V n x
householder q x = x - (one + one) * ((q <•> x) / quadrance q) *. q

orthogonalize ::
  (KnownNat n, Field x, Conjugate x) =>
  [V n x] -> [V n x]
orthogonalize [] = []
orthogonalize (v0 : vs0) = gramSchmidt1 [v0] vs0
 where
  gramSchmidt1 us [] = List.reverse us
  gramSchmidt1 us (v : vs) =
    let u = v + sumOn (negative . (`projection` v)) us
     in gramSchmidt1 (u : us) vs

orthonormalize ::
  (KnownNat n, Field x, Conjugate x, Root x) =>
  [V n x] -> [V n x]
orthonormalize vs = morphism
  do \u -> reciprocal (2 √ quadrance u) *. u
  do orthogonalize vs

newtype M m n x = M {unM :: V m (V n x)} deriving (Data.Functor)
instance Morphisms (->) (->) (M m n) where
  morphism :: forall x y. (x -> y) -> M m n x -> M m n y
  morphism x_y (M a) = M (morphism (morphism x_y :: V n x -> V n y) a)
instance Morphisms (Ix (Natural, Natural)) (->) (M m n) where
  morphism :: forall x y. Ix (Natural, Natural) x y -> M m n x -> M m n y
  morphism (Ix i_x_y) (M a) =
    M (morphism (Ix \i -> morphism (Ix \j -> i_x_y (i, j))) a)
instance (KnownNat m, KnownNat n) => Folds (->) (->) (M m n) where
  foldWith :: forall x s. (Monoid s) => (x -> s) -> M m n x -> s
  foldWith f (M a) = foldWith (foldWith f :: V n x -> s) a
instance (KnownNat m, KnownNat n) => Traversals (->) (->) (M m n) where
  traverse ::
    forall g x y. (Applicative g) => (x -> g y) -> M m n x -> g (M m n y)
  traverse x_gy (M a) = morphism M (traverse (traverse x_gy :: V n x -> g (V n y)) a)
instance (KnownNat m, KnownNat n) => Data.Foldable (M m n) where
  foldMap :: forall x s. (Monoid s) => (x -> s) -> M m n x -> s
  foldMap f (M a) = foldWith (foldWith f :: V n x -> s) a
instance (KnownNat m, KnownNat n) => Data.Traversable (M m n) where
  traverse ::
    forall f x y. (Control.Applicative f) => (x -> f y) -> M m n x -> f (M m n y)
  traverse f (M a) = M Data.<$> Data.traverse (Data.traverse f) a
instance (KnownNat m, KnownNat n, Eq x) => Eq (M m n x) where
  (==) :: M m n x -> M m n x -> Bool
  M a == M b =
    List.all
      (\i -> List.all (\j -> a ! i ! j == b ! i ! j) (dimensions (Proxy @n)))
      (dimensions (Proxy @m))
instance (KnownNat m, KnownNat n, Ord x) => Ord (M m n x) where
  compare :: M m n x -> M m n x -> Ordering
  compare (M a) (M b) =
    flip foldWith (dimensions (Proxy @m)) \i ->
      flip foldWith (dimensions (Proxy @n)) \j ->
        compare (a ! i ! j) (b ! i ! j)

instance (KnownNat m, KnownNat n, Show x) => Show (M m n x) where
  show :: M m n x -> [Char]
  show (M a) = "M {" <> foldWith out (dimensions (Proxy @m)) <> "}"
   where
    out i = "{" <> foldWith (inn i) (dimensions (Proxy @n)) <> "}"
    inn i j = " " <> show (a ! i ! j) <> " "

instance (KnownNat m, KnownNat n) => Folds (Ix (Natural, Natural)) (->) (M m n) where
  foldWith :: (Monoid z) => Ix (Natural, Natural) x z -> M m n x -> z
  foldWith (Ix ij_x_z) (M (V vs)) =
    foldWith (Ix \i -> foldWith (Ix \j -> ij_x_z (i, j))) vs
instance (KnownNat m, KnownNat n) => Traversals (Ix (Natural, Natural)) (->) (M m n) where
  traverse ::
    (Applicative f) =>
    Ix (Natural, Natural) x (f y) -> M m n x -> f (M m n y)
  traverse (Ix ij_x_z) (M a) =
    morphism M (traverse (Ix \i -> traverse (Ix \j -> ij_x_z (i, j))) a)

instance
  (KnownNat m, KnownNat n, Addition x x x) =>
  Addition (M m n x) (M m n x) (M m n x)
  where
  (+.) :: M m n x -> M m n x -> M m n x
  M a +. M b = M (a +. b)
instance
  (KnownNat m, KnownNat n, Additive x) =>
  Additive (M m n x)
  where
  zero :: M m n x
  zero = M (pure zero)
instance
  (KnownNat m, KnownNat n, AdditiveAbelian x) =>
  AdditiveAbelian (M m n x)
instance
  (KnownNat m, KnownNat n, Subtraction x x x) =>
  Subtraction (M m n x) (M m n x) (M m n x)
  where
  (-.) :: M m n x -> M m n x -> M m n x
  M a -. M b = M (a -. b)
instance
  (KnownNat m, KnownNat n, AdditiveGroup x) =>
  AdditiveGroup (M m n x)
  where
  negative :: M m n x -> M m n x
  negative = morphism negative

instance
  (KnownNat m, KnownNat n, Multiplication x x x) =>
  Multiplication x (M m n x) (M m n x)
  where
  (*.) :: x -> M m n x -> M m n x
  x *. M a = M (morphism (x *.) a)

instance
  (KnownNat m, KnownNat n, Multiplication x x x) =>
  Multiplication (M m n x) x (M m n x)
  where
  (*.) :: M m n x -> x -> M m n x
  M a *. x = M (morphism (*. x) a)

instance
  (KnownNat m, KnownNat n, Division x x x) =>
  Division (M m n x) x (M m n x)
  where
  (/.) :: M m n x -> x -> M m n x
  M a /. x = M (morphism (/. x) a)

instance
  ( KnownNat m
  , KnownNat n
  , AdditiveAbelian x
  , Multiplication x x x
  ) =>
  Multiplication (M m n x) (V n x) (V m x)
  where
  (*.) :: M m n x -> V n x -> V m x
  M a *. V v = vn (natVal (Proxy @m)) \i -> sum (zipWith (*.) (a ! i) (V v))

instance
  ( KnownNat m
  , KnownNat n
  , KnownNat p
  , AdditiveAbelian x
  , Multiplication x x x
  ) =>
  Multiplication (M m n x) (M n p x) (M m p x)
  where
  (*.) :: M m n x -> M n p x -> M m p x
  M a *. M b = M do
    vn (natVal (Proxy @m)) \i -> vn (natVal (Proxy @p)) \j ->
      sumOn (\k -> a ! i ! k * b ! k ! j) (dimensions (Proxy @n))

hadamard :: (Multiplication x x x) => M m n x -> M m n x -> M m n x
hadamard (M a) (M b) = M (zipWith (zipWith (*.)) a b)

kronecker ::
  forall m n p q x.
  ( KnownNat p
  , KnownNat q
  , KnownNat (m * p)
  , KnownNat (n * q)
  , Multiplication x x x
  ) =>
  M m n x -> M p q x -> M (m * p) (n * q) x
kronecker (M a) (M b) = M do
  vn (natVal (Proxy @(m * p))) \i -> vn (natVal (Proxy @(n * q))) \j ->
    let p = natVal (Proxy @p)
        q = natVal (Proxy @q)
        (ia, ib) = euclidean i p
        (ja, jb) = euclidean j q
     in a ! ia ! ja * b ! ib ! jb

instance
  (KnownNat n, AdditiveAbelian x, Multiplicative x) =>
  Multiplicative (M n n x)
  where
  one :: M n n x
  one = M $ V $ flip Vector.unfoldr 0 \i -> do
    guard (i < natVal (Proxy @n))
    pure (setV i one zero, succ i)

instance (KnownNat m, KnownNat n, From y x) => From y (Scalar (M m n x)) where
  from :: y -> Scalar (M m n x)
  from y = ScalarM (from y)

instance (KnownNat m, KnownNat n, Addition x x x) => Addition (Scalar (M m n x)) (Scalar (M m n x)) (Scalar (M m n x)) where
  (+.) :: Scalar (M m n x) -> Scalar (M m n x) -> Scalar (M m n x)
  ScalarM x +. ScalarM y = ScalarM (x + y)
instance (KnownNat m, KnownNat n, Subtraction x x x) => Subtraction (Scalar (M m n x)) (Scalar (M m n x)) (Scalar (M m n x)) where
  (-.) :: Scalar (M m n x) -> Scalar (M m n x) -> Scalar (M m n x)
  ScalarM x -. ScalarM y = ScalarM (x - y)
instance (KnownNat m, KnownNat n, Multiplication x x x) => Multiplication (Scalar (M m n x)) (Scalar (M m n x)) (Scalar (M m n x)) where
  (*.) :: Scalar (M m n x) -> Scalar (M m n x) -> Scalar (M m n x)
  ScalarM x *. ScalarM y = ScalarM (x * y)
instance (KnownNat m, KnownNat n, Division x x x) => Division (Scalar (M m n x)) (Scalar (M m n x)) (Scalar (M m n x)) where
  (/.) :: Scalar (M m n x) -> Scalar (M m n x) -> Scalar (M m n x)
  ScalarM x /. ScalarM y = ScalarM (x / y)
instance
  (KnownNat m, KnownNat n, Multiplication x x x) =>
  Multiplication (Scalar (M m n x)) (M m n x) (M m n x)
  where
  (*.) :: Scalar (M m n x) -> M m n x -> M m n x
  ScalarM x *. a = x *. a
instance
  (KnownNat m, KnownNat n, Multiplication x x x) =>
  Multiplication (M m n x) (Scalar (M m n x)) (M m n x)
  where
  (*.) :: M m n x -> Scalar (M m n x) -> M m n x
  a *. ScalarM x = a *. x
instance
  (KnownNat m, KnownNat n, Division x x x) =>
  Division (M m n x) (Scalar (M m n x)) (M m n x)
  where
  (/.) :: M m n x -> Scalar (M m n x) -> M m n x
  a /. ScalarM x = a /. x
instance
  (KnownNat m, KnownNat n, Power x r x) =>
  Power (Scalar (M m n x)) r (Scalar (M m n x))
  where
  (^) :: Scalar (M m n x) -> r -> Scalar (M m n x)
  ScalarM x ^ r = ScalarM (x ^ r)
instance (KnownNat m, KnownNat n, Absolute x x) => Absolute (Scalar (M m n x)) x where
  absolute :: Scalar (M m n x) -> x
  absolute (ScalarM k) = absolute k
instance (KnownNat m, KnownNat n, Absolute x x) => Absolute (Scalar (M m n x)) (Scalar (M m n x)) where
  absolute :: Scalar (M m n x) -> Scalar (M m n x)
  absolute (ScalarM k) = ScalarM (absolute k)

instance (KnownNat m, KnownNat n, Semiring x) => Semiring (Scalar (M m n x))
instance (KnownNat m, KnownNat n, Ring x) => Ring (Scalar (M m n x))

instance
  ( KnownNat m
  , KnownNat n
  , Eq x
  , Ring x
  ) =>
  Module (M m n x)
  where
  newtype Scalar (M m n x) = ScalarM {unScalar :: x}
    deriving newtype
      ( Eq
      , Ord
      , Show
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
instance
  ( KnownNat n
  , AdditiveAbelian x
  , Multiplicative x
  ) =>
  Distributive (M n n x)
instance
  ( KnownNat n
  , Eq x
  , Ring x
  , MultiplicativeAbelian x
  ) =>
  Bilinear (M n n x)
  where
  (•) :: M n n x -> M n n x -> Scalar (M n n x)
  a • b = trace (transpose a * b)
instance
  ( KnownNat m
  , KnownNat n
  , Eq x
  , Ring x
  , MultiplicativeAbelian x
  , Conjugate x
  ) =>
  Sesquilinear (M m n x)
  where
  (<•>) :: M m n x -> M m n x -> Scalar (M m n x)
  a <•> b = case trace @(M m m) (a *. adjoint @x @(M m n) @(M n m) b) of
    ScalarM x -> ScalarM x
instance 
  ( KnownNat n
  , Eq x
  , Ring x
  , MultiplicativeAbelian x
  , Conjugate x
  ) =>
  InnerProduct (M n n x)
instance
  (KnownNat n, Eq x, Ring x) =>
  Algebra (M n n x)
instance
  (KnownNat n, Eq x, Ring x) =>
  Unital (M n n x)
instance
  (KnownNat n, Eq x, Ring x) =>
  AssociativeAlgebra (M n n x)
instance (KnownNat m, KnownNat n, Eq x, Field x) => Vector (M m n x)

outer ::
  ( KnownNat m
  , KnownNat n
  , AdditiveAbelian x
  , Multiplication x x x
  ) =>
  V m x -> V n x -> M m n x
outer u v = column u *. row v

instance (KnownNat m, KnownNat n) => Matrix (M m n) (M n m) x where
  transpose :: M m n x -> M n m x
  transpose (M a) = M $ vn n \i -> vn m \j -> a ! j ! i
   where
    m = natVal (Proxy @m)
    n = natVal (Proxy @n)
instance (KnownNat n, Eq x, MultiplicativeAbelian x, Ring x) => Square (M n n) x where
  trace :: M n n x -> Scalar (M n n x)
  trace (M a) = ScalarM do
    sumOn (\k -> a ! k ! k) (dimensions (Proxy @n))
  determinant :: M n n x -> Scalar (M n n x)
  determinant (M a) = ScalarM do
    flip
      sumOn
      (List.permutations (dimensions (Proxy @n)))
      \p ->
        signature p
          * productOn
            (\x -> a ! x ! (p List.!! from x))
            (dimensions (Proxy @n))
   where
    signature p = (\cnt -> if even cnt then one else negative one) $ count id do
      x <- dimensions (Proxy @n)
      y <- List.dropWhile (<= x) (dimensions (Proxy @n))
      pure $ (p List.!! from x) > (p List.!! from y)

instance
  (KnownNat n, AdditiveAbelian x, Multiplicative x) =>
  Power (M n n x) Natural (M n n x)
  where
  (^) :: M n n x -> Natural -> M n n x
  a ^ n = case n of
    0 -> one
    _ -> a * a ^ pred n

rows ::
  forall m n x.
  (KnownNat m, KnownNat n) =>
  M m n x -> [V n x]
rows (M a) = morphism (a !) (dimensions (Proxy @m))

unrows :: forall m n x. (KnownNat m, KnownNat n) => [V n x] -> Maybe (M m n x)
unrows vs
  | count (const True) vs == natVal (Proxy @m) =
      Just . M . V $ Vector.fromList vs
  | otherwise = Nothing

row :: V n x -> M 1 n x
row x = M (pure x)

columns ::
  forall m n x.
  (KnownNat m, KnownNat n) =>
  M m n x -> [V m x]
columns a =
  let M aT = transpose @(M m n) @(M n m) a
   in morphism (aT !) (dimensions (Proxy @n))

uncolumns ::
  forall m n x.
  (KnownNat m, KnownNat n) =>
  [V m x] -> Maybe (M m n x)
uncolumns vs
  | count (const True) vs == n = (Just . M) do
      vn (natVal (Proxy @m)) \i -> vn n \j -> (vs List.!! from j) ! i
  | otherwise = Nothing
 where
  n = natVal (Proxy @n)

column :: forall m x. (KnownNat m) => V m x -> M m 1 x
column x = fromJust (uncolumns @m @1 [x])

toLists ::
  forall m n x.
  (KnownNat m, KnownNat n) =>
  M m n x -> [[x]]
toLists = morphism toList . rows

qr ::
  forall n x.
  (KnownNat n, Eq x, Conjugate x, Root x) =>
  M n n x -> (M n n x, M n n x)
qr a =
  let q = fromJust . uncolumns $ orthonormalize (columns a)
   in (q, transpose q * a)

lu ::
  forall n x.
  (KnownNat n, Field x) =>
  M n n x -> (M n n x, M n n x)
lu (M a) = build 0 zero one
 where
  n = natVal (Proxy @n)
  buildLVal !i !j (M l) (M u) =
    let go !k !s
          | k == j = s
          | otherwise = go (succ k) (s + l ! i ! k * u ! k ! j)
        s' = go zero zero
     in M $ vn n \i' -> vn n \j' -> 
          if i == i' && j == j' then a ! i' ! j' - s' else l ! i' ! j'
  buildL !i !j l u
    | i == natVal (Proxy @n) = l
    | otherwise = buildL (succ i) j (buildLVal i j l u) u
  buildUVal !i !j (M l) (M u) =
    let go !k !s
          | k == j = s
          | otherwise = go (succ k) (s + l ! j ! k * u ! k ! i)
        s' = go zero zero
     in M $ vn n \i' -> vn n \j' ->
          if i == j' && j == i' then (a ! j ! i - s') / l ! j ! j else u ! i' ! j'
  buildU !i !j l u
    | i == natVal (Proxy @n) = u
    | otherwise = buildU (succ i) j l (buildUVal i j l u)
  build !j l u
    | j == natVal (Proxy @n) = (l, u)
    | otherwise =
        let l' = buildL j j l u
            u' = buildU j j l' u
         in build (succ j) l' u'

system :: forall n x. (KnownNat n, Field x) => M n n x -> V n x -> V n x
system a y = let (l, u) = lu a in backward u (forward l y)
 where
  n = natVal (Proxy @n)
  forward (M l) x =
    let coeff !i !j !s z
          | i == j = s
          | otherwise = coeff i (succ j) (s + l ! i ! j * z ! j) z
        go :: Natural -> V n x -> V n x
        go !i z
          | i == natVal (Proxy @n) = z
          | otherwise =
              go
                (succ i)
                ( vn n \i' ->
                    if i == i'
                      then (x ! i - coeff i 0 zero z) / l ! i ! i
                      else z ! i'
                )
     in go 0 zero
  backward (M u) x =
    let coeff !i !j !s z
          | j == n = s
          | otherwise = coeff i (succ j) (s + u ! i ! j * z ! j) z
        go !i z
          | i == 0 = z
          | otherwise =
              go
                (pred i)
                ( vn n \i' ->
                    if i == succ i'
                      then (x ! i' - coeff i' (succ i') zero z) / u ! i' ! i'
                      else z ! i'
                )
     in go n zero

minor ::
  forall n x.
  (KnownNat n, Eq x, MultiplicativeAbelian x, Ring x) =>
  Natural -> Natural -> M (n + 1) (n + 1) x -> Scalar (M n n x)
minor i_ j_ (M a) = determinant @(M n n) $ M do
  vn (natVal (Proxy @n)) \i -> vn (natVal (Proxy @n)) \j ->
    a ! (if i < i_ then i else i + one) ! (if j < j_ then j else j + one)

cofactor ::
  forall n x.
  (KnownNat n, Eq x, MultiplicativeAbelian x, Ring x, Power x Natural x) =>
  Natural -> Natural -> M (n + 1) (n + 1) x -> Scalar (M n n x)
cofactor i_ j_ a = ScalarM @n @n (negative @x one) ^ (i_ + j_) * minor i_ j_ a

cofactorMatrix ::
  forall n x.
  ( KnownNat n
  , Eq x
  , MultiplicativeAbelian x
  , Ring x
  , Power x Natural x
  ) =>
  M (n + 1) (n + 1) x -> M (n + 1) (n + 1) x
cofactorMatrix a = M $ vn n \i -> vn n \j -> (cofactor i j a).unScalar
 where
  n = natVal (Proxy @n)

adjugate ::
  ( KnownNat n
  , KnownNat (n + 1)
  , Eq x
  , MultiplicativeAbelian x
  , Ring x
  , Power x Natural x
  ) =>
  M (n + 1) (n + 1) x -> M (n + 1) (n + 1) x
adjugate = transpose . cofactorMatrix

characteristicPolynomial ::
  forall n x.
  (KnownNat n, Eq x, MultiplicativeAbelian x, Ring x) => M n n x -> List1 x
characteristicPolynomial a = (determinant (tI - morphism pure a)).unScalar
 where
  n = natVal (Proxy @n)
  tI = M @n @n $ vn n \i -> vn n \j -> if i == j then zero :|| Sole one else Sole zero

diagonal :: forall n x. (KnownNat n, Additive x, Eq x) => M n n x -> Bool
diagonal (M a) = Data.all
  do \(i, j) -> i == j || a ! i ! j == zero
  do Control.join (Control.liftM2 (,)) (dimensions (Proxy @n))

upperTriangular ::
  forall n x. (KnownNat n, Additive x, Eq x) => M n n x -> Bool
upperTriangular (M a) = Data.all
  do \(i, j) -> i <= j || a ! i ! j == zero
  do Control.join (Control.liftM2 (,)) (dimensions (Proxy @n))

lowerTriangular ::
  forall n x. (KnownNat n, Additive x, Eq x) => M n n x -> Bool
lowerTriangular (M a) = Data.all
  do \(i, j) -> i >= j || a ! i ! j == zero
  do Control.join (Control.liftM2 (,)) (dimensions (Proxy @n))

symmetric :: forall n x. (KnownNat n, Eq x) => M n n x -> Bool
symmetric (M a) = Data.all
  do \(i, j) -> a ! i ! j == a ! j ! i
  do Control.join (Control.liftM2 (,)) (dimensions (Proxy @n))

hermitian :: forall n x. (KnownNat n, Eq x, Conjugate x) => M n n x -> Bool
hermitian (M a) = Data.all
  do \(i, j) -> a ! i ! j == conjugate (a ! j ! i)
  do Control.join (Control.liftM2 (,)) (dimensions (Proxy @n))

vn :: Natural -> (Natural -> x) -> V n x
vn n f = V (Vector.generate (from n) (f . from))

vnM :: (Control.Monad m) => Natural -> (Natural -> m x) -> m (V n x)
vnM n f = V Data.<$> Vector.generateM (from n) (f . from)

v1 :: forall x. x -> V 1 x
v1 x = V (Vector.singleton x)

withV1 :: V 1 x -> (x -> y) -> y
withV1 v f = f (v ! 0)

v2 :: forall x. x -> x -> V 2 x
v2 x y = V (Vector.singleton x <> Vector.singleton y)

withV2 :: V 2 x -> (x -> x -> y) -> y
withV2 v f = f (v ! 0) (v ! 1)

v3 :: forall x. x -> x -> x -> V 3 x
v3 x y z = V (Vector.fromList [x, y, z])

withV3 :: V 3 x -> (x -> x -> x -> y) -> y
withV3 v f = f (v ! 0) (v ! 1) (v ! 2)

v4 :: forall x. x -> x -> x -> x -> V 4 x
v4 x y z w = V (Vector.fromList [x, y, z, w])

withV4 :: V 4 x -> (x -> x -> x -> x -> y) -> y
withV4 v f = f (v ! 0) (v ! 1) (v ! 2) (v ! 3)

m22 :: forall x. x -> x -> x -> x -> M 2 2 x
m22 a b c d = M $ V do
  Vector.fromList
    [ V $ Vector.fromList [a, b]
    , V $ Vector.fromList [c, d]
    ]

m23 :: forall x. x -> x -> x -> x -> x -> x -> M 2 3 x
m23 a b c d e f = M $ V do
  Vector.fromList
    [ V $ Vector.fromList [a, b, c]
    , V $ Vector.fromList [d, e, f]
    ]

m32 :: forall x. x -> x -> x -> x -> x -> x -> M 3 2 x
m32 a b c d e f = M $ V do
  Vector.fromList
    [ V $ Vector.fromList [a, b]
    , V $ Vector.fromList [c, d]
    , V $ Vector.fromList [e, f]
    ]

m24 :: forall x. x -> x -> x -> x -> x -> x -> x -> x -> M 2 4 x
m24 a b c d e f g h = M $ V do
  Vector.fromList
    [ V $ Vector.fromList [a, b, c, d]
    , V $ Vector.fromList [e, f, g, h]
    ]

m42 :: forall x. x -> x -> x -> x -> x -> x -> x -> x -> M 4 2 x
m42 a b c d e f g h = M $ V do
  Vector.fromList
    [ V $ Vector.fromList [a, b]
    , V $ Vector.fromList [c, d]
    , V $ Vector.fromList [e, f]
    , V $ Vector.fromList [g, h]
    ]

m33 :: forall x. x -> x -> x -> x -> x -> x -> x -> x -> x -> M 3 3 x
m33 a b c d e f g h i = M $ V do
  Vector.fromList
    [ V $ Vector.fromList [a, b, c]
    , V $ Vector.fromList [d, e, f]
    , V $ Vector.fromList [g, h, i]
    ]

m34 ::
  forall x. x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> M 3 4 x
m34 a b c d e f g h i j k l = M $ V do
  Vector.fromList
    [ V $ Vector.fromList [a, b, c, d]
    , V $ Vector.fromList [e, f, g, h]
    , V $ Vector.fromList [i, j, k, l]
    ]

m43 ::
  forall x. x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> M 4 3 x
m43 a b c d e f g h i j k l = M $ V do
  Vector.fromList
    [ V $ Vector.fromList [a, b, c]
    , V $ Vector.fromList [d, e, f]
    , V $ Vector.fromList [g, h, i]
    , V $ Vector.fromList [j, k, l]
    ]

m44 ::
  forall x.
  x ->
  x ->
  x ->
  x ->
  x ->
  x ->
  x ->
  x ->
  x ->
  x ->
  x ->
  x ->
  x ->
  x ->
  x ->
  x ->
  M 4 4 x
m44 a b c d e f g h i j k l m n o p = M $ V do
  Vector.fromList
    [ V $ Vector.fromList [a, b, c, d]
    , V $ Vector.fromList [e, f, g, h]
    , V $ Vector.fromList [i, j, k, l]
    , V $ Vector.fromList [m, n, o, p]
    ]
