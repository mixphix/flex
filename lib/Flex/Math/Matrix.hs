{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Flex.Math.Matrix
  ( Matrix (transpose)
  , adjoint
  , Square (trace, determinant)
  , M (M, unM, M22, M23, M24, M32, M33, M34, M42, M43, M44)
  , rows
  , unrows
  , row
  , columns
  , uncolumns
  , column
  , outerproduct
  , permanent
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
  , gramMatrix
  , diagonal
  , upperTriangular
  , lowerTriangular
  , symmetric
  , hermitian
  , Scalar (..)
  , Signature (..)
  , Term (..)
  , Laws (..)

    -- ** Re-exports
  , KnownNat
  , natVal
  , Proxy (Proxy)
  , Finite
  , getFinite
  ) where

import Flex.Math.Algebra
import Flex.Math.Category
import Flex.Math.Foldable
import Flex.Math.Module
import Flex.Math.Numbers
import Flex.Math.Optics
import Flex.Math.Polynomial (Polynomial, data Œ)
import Flex.Math.Structure
import Flex.Math.Vector

import Control.Applicative qualified as Control
import Data.Bool
import Data.Enum
import Data.Eq
import Data.Finite (Finite, finites, getFinite)
import Data.Foldable qualified as Data
import Data.Function (const, flip, on)
import Data.Functor qualified as Data
import Data.List qualified as List
import Data.Maybe
import Data.Ord (Ord (..), Ordering (..))
import Data.Proxy
import Data.Semigroup (Endo (..))
import Data.Traversable qualified as Data
import Flex.Math.Polynomial qualified as Polynomial
import GHC.Generics (Generic)
import GHC.TypeNats
import Text.Show

class (Traversable m, Traversable n) => Matrix m n x where
  transpose :: m x -> n x

adjoint :: (Conjugate x, Matrix m n x) => m x -> n x
adjoint = morphism conjugate . transpose

class
  ( Matrix m m x
  , Eq (Scalar (m x))
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
  operations =
    TermSquare . \case
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

newtype M m n x = M {unM :: V m (V n x)} deriving (Data.Functor)
instance Morphisms (->) (->) (M m n) where
  morphism :: forall x y. (x -> y) -> M m n x -> M m n y
  morphism x_y (M a) = M (morphism (morphism x_y :: V n x -> V n y) a)
  {-# INLINE morphism #-}
instance Morphisms (Ix (Natural, Natural)) (->) (M m n) where
  morphism :: forall x y. Ix (Natural, Natural) x y -> M m n x -> M m n y
  morphism (Ix i_x_y) (M a) =
    M (morphism (Ix \ !i -> morphism (Ix \ !j -> i_x_y (i, j))) a)
  {-# INLINE morphism #-}
instance (KnownNat m, KnownNat n) => Folds (->) (->) (M m n) where
  foldWith :: forall x z. (Monoid z) => (x -> z) -> M m n x -> z
  foldWith x_z (M a) = foldWith (foldWith x_z :: V n x -> z) a
  {-# INLINE foldWith #-}
instance (KnownNat m, KnownNat n) => Folds (Ix (Natural, Natural)) (->) (M m n) where
  foldWith :: (Monoid z) => Ix (Natural, Natural) x z -> M m n x -> z
  foldWith (Ix ij_x_z) (M vs) =
    foldWith (Ix \ !i -> foldWith (Ix \ !j -> ij_x_z (i, j))) vs
  {-# INLINE foldWith #-}
instance (KnownNat m, KnownNat n) => Traversals (->) (->) (M m n) where
  traverse ::
    forall g x y. (Applicative g) => (x -> g y) -> M m n x -> g (M m n y)
  traverse x_gy (M a) = morphism M (traverse (traverse x_gy :: V n x -> g (V n y)) a)
  {-# INLINE traverse #-}
instance (KnownNat m, KnownNat n) => Traversals (Ix (Natural, Natural)) (->) (M m n) where
  traverse ::
    (Applicative f) =>
    Ix (Natural, Natural) x (f y) -> M m n x -> f (M m n y)
  traverse (Ix ij_x_z) (M a) =
    morphism M (traverse (Ix \ !i -> traverse (Ix \ !j -> ij_x_z (i, j))) a)
  {-# INLINE traverse #-}
instance (KnownNat m, KnownNat n) => Data.Foldable (M m n) where
  foldMap :: forall x z. (Monoid z) => (x -> z) -> M m n x -> z
  foldMap x_z (M a) = foldWith (foldWith x_z :: V n x -> z) a
  {-# INLINE foldMap #-}
instance (KnownNat m, KnownNat n) => Data.Traversable (M m n) where
  traverse ::
    forall f x y. (Control.Applicative f) => (x -> f y) -> M m n x -> f (M m n y)
  traverse x_gy (M a) = M Data.<$> Data.traverse (Data.traverse x_gy) a
  {-# INLINE traverse #-}
instance (KnownNat m, KnownNat n, Eq x) => Eq (M m n x) where
  (==) :: M m n x -> M m n x -> Bool
  M a == M b = a == b
  {-# INLINE (==) #-}
instance (KnownNat m, KnownNat n, Ord x) => Ord (M m n x) where
  compare :: M m n x -> M m n x -> Ordering
  compare (M a) (M b) = compare a b
  {-# INLINE compare #-}

instance (KnownNat m, KnownNat n, Show x) => Show (M m n x) where
  showsPrec :: Int -> M m n x -> ShowS
  showsPrec prec (M a) = showParen (prec > 10) do
    showString "M {"
      . appEndo (foldWith (Endo . inside) a)
      . showString "}"
   where
    inside ::
      forall k. (KnownNat k, Show x) => V k x -> ShowS
    inside v =
      showString "{ "
        . appEndo (foldWith (\x -> Endo do showsPrec 11 x . showString " ") v)
        . showString "}"
    {-# INLINE inside #-}
  {-# INLINE showsPrec #-}

instance
  (KnownNat m, KnownNat n, Addition x x x) =>
  Addition (M m n x) (M m n x) (M m n x)
  where
  (+.) :: M m n x -> M m n x -> M m n x
  M a +. M b = M (a +. b)
  {-# INLINE (+.) #-}
instance
  (KnownNat m, KnownNat n, Additive x) =>
  Additive (M m n x)
  where
  zero :: M m n x
  zero = M (pure zero)
  {-# INLINE zero #-}
instance
  (KnownNat m, KnownNat n, AdditiveAbelian x) =>
  AdditiveAbelian (M m n x)
instance
  (KnownNat m, KnownNat n, Subtraction x x x) =>
  Subtraction (M m n x) (M m n x) (M m n x)
  where
  (-.) :: M m n x -> M m n x -> M m n x
  M a -. M b = M (a -. b)
  {-# INLINE (-.) #-}
instance
  (KnownNat m, KnownNat n, AdditiveGroup x) =>
  AdditiveGroup (M m n x)
  where
  negative :: M m n x -> M m n x
  negative = morphism negative
  {-# INLINE negative #-}

instance
  (KnownNat m, KnownNat n, Multiplication x x x) =>
  Multiplication x (M m n x) (M m n x)
  where
  (*.) :: x -> M m n x -> M m n x
  x *. M a = M (morphism (x *.) a)
  {-# INLINE (*.) #-}

instance
  (KnownNat m, KnownNat n, Multiplication x x x) =>
  Multiplication (M m n x) x (M m n x)
  where
  (*.) :: M m n x -> x -> M m n x
  M a *. x = M (morphism (*. x) a)
  {-# INLINE (*.) #-}

instance
  (KnownNat m, KnownNat n, Division x x x) =>
  Division (M m n x) x (M m n x)
  where
  (/.) :: M m n x -> x -> M m n x
  M a /. x = M (morphism (/. x) a)
  {-# INLINE (/.) #-}

instance
  ( KnownNat m
  , KnownNat n
  , AdditiveAbelian x
  , Multiplication x x x
  ) =>
  Multiplication (M m n x) (V n x) (V m x)
  where
  (*.) :: M m n x -> V n x -> V m x
  M a *. v = vn @m \i -> sum (liftA2 (*.) (a ! i) v)
  {-# INLINE (*.) #-}

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
    vn @m \i -> vn @p \j ->
      sumOn (\k -> a ! i ! k * b ! k ! j) [0 .. natVal (Proxy @n) - 1]
  {-# INLINE (*.) #-}

hadamard :: (Multiplication x x x) => M m n x -> M m n x -> M m n x
hadamard (M a) (M b) = M (liftA2 (liftA2 (*)) a b)
{-# INLINE hadamard #-}

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
  vn @(m * p) \i -> vn @(n * q) \j ->
    let p = natVal (Proxy @p)
        q = natVal (Proxy @q)
        (ia, ib) = euclidean i p
        (ja, jb) = euclidean j q
     in a ! ia ! ja * b ! ib ! jb
{-# INLINE kronecker #-}

instance
  (KnownNat n, AdditiveAbelian x, Multiplicative x) =>
  Multiplicative (M n n x)
  where
  one :: M n n x
  one = M (vn @n \i -> vn @n \j -> if i == j then one else zero)
  {-# INLINE one #-}

instance (KnownNat m, KnownNat n, From y x) => From y (Scalar (M m n x)) where
  from :: y -> Scalar (M m n x)
  from y = ScalarM (from y)
  {-# INLINE from #-}

instance
  (KnownNat m, KnownNat n, Addition x x x) =>
  Addition (Scalar (M m n x)) (Scalar (M m n x)) (Scalar (M m n x))
  where
  (+.) :: Scalar (M m n x) -> Scalar (M m n x) -> Scalar (M m n x)
  ScalarM x +. ScalarM y = ScalarM (x + y)
  {-# INLINE (+.) #-}
instance
  (KnownNat m, KnownNat n, Subtraction x x x) =>
  Subtraction (Scalar (M m n x)) (Scalar (M m n x)) (Scalar (M m n x))
  where
  (-.) :: Scalar (M m n x) -> Scalar (M m n x) -> Scalar (M m n x)
  ScalarM x -. ScalarM y = ScalarM (x - y)
  {-# INLINE (-.) #-}
instance
  (KnownNat m, KnownNat n, Multiplication x x x) =>
  Multiplication (Scalar (M m n x)) (Scalar (M m n x)) (Scalar (M m n x))
  where
  (*.) :: Scalar (M m n x) -> Scalar (M m n x) -> Scalar (M m n x)
  ScalarM x *. ScalarM y = ScalarM (x * y)
  {-# INLINE (*.) #-}
instance
  (KnownNat m, KnownNat n, Division x x x) =>
  Division (Scalar (M m n x)) (Scalar (M m n x)) (Scalar (M m n x))
  where
  (/.) :: Scalar (M m n x) -> Scalar (M m n x) -> Scalar (M m n x)
  ScalarM x /. ScalarM y = ScalarM (x / y)
  {-# INLINE (/.) #-}
instance
  (KnownNat m, KnownNat n, Multiplication x x x) =>
  Multiplication (Scalar (M m n x)) (M m n x) (M m n x)
  where
  (*.) :: Scalar (M m n x) -> M m n x -> M m n x
  ScalarM x *. a = x *. a
  {-# INLINE (*.) #-}
instance
  (KnownNat m, KnownNat n, Multiplication x x x) =>
  Multiplication (M m n x) (Scalar (M m n x)) (M m n x)
  where
  (*.) :: M m n x -> Scalar (M m n x) -> M m n x
  a *. ScalarM x = a *. x
  {-# INLINE (*.) #-}
instance
  (KnownNat m, KnownNat n, Division x x x) =>
  Division (M m n x) (Scalar (M m n x)) (M m n x)
  where
  (/.) :: M m n x -> Scalar (M m n x) -> M m n x
  a /. ScalarM x = a /. x
  {-# INLINE (/.) #-}
instance
  (KnownNat m, KnownNat n, Power x r x) =>
  Power (Scalar (M m n x)) r (Scalar (M m n x))
  where
  (^) :: Scalar (M m n x) -> r -> Scalar (M m n x)
  ScalarM x ^ r = ScalarM (x ^ r)
  {-# INLINE (^) #-}
instance (KnownNat m, KnownNat n, Absolute x x) => Absolute (Scalar (M m n x)) x where
  absolute :: Scalar (M m n x) -> x
  absolute (ScalarM k) = absolute k
  {-# INLINE absolute #-}
instance
  (KnownNat m, KnownNat n, Absolute x x) =>
  Absolute (Scalar (M m n x)) (Scalar (M m n x))
  where
  absolute :: Scalar (M m n x) -> Scalar (M m n x)
  absolute (ScalarM k) = ScalarM (absolute k)
  {-# INLINE absolute #-}

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
  {-# INLINE (•) #-}
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
  {-# INLINE (<•>) #-}
instance (KnownNat m, KnownNat n, Eq x, Field x) => Vector (M m n x)
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

instance (KnownNat m, KnownNat n) => Indices (M m n x) where
  type Index (M m n x) = (Natural, Natural)
  type Value (M m n x) = x
  index :: (Natural, Natural) -> Traversal' (M m n x) x
  index (i, j) x_fx' v@(M vs)
    | from i < n =
        morphism
          (\x' -> M ((index i . index j .~ x') vs))
          (x_fx' (vs ! i ! j))
    | otherwise = pure v
   where
    n = natVal (Proxy @n)
  {-# INLINE index #-}

outerproduct ::
  ( KnownNat m
  , KnownNat n
  , AdditiveAbelian x
  , Multiplication x x x
  ) =>
  V m x -> V n x -> M m n x
outerproduct u v = column u *. row v
{-# INLINE outerproduct #-}

instance (KnownNat m, KnownNat n) => Matrix (M m n) (M n m) x where
  transpose :: M m n x -> M n m x
  transpose (M a) = M (vn @n \i -> vn @m \j -> a ! j ! i)
  {-# INLINE transpose #-}
instance (KnownNat n, Eq x, MultiplicativeAbelian x, Ring x) => Square (M n n) x where
  trace :: M n n x -> Scalar (M n n x)
  trace (M a) = ScalarM (sumOn (\k -> a ! from k ! from k) (finites @n))
  {-# INLINE trace #-}
  determinant :: M n n x -> Scalar (M n n x)
  determinant (M a) = ScalarM do
    flip sumOn (List.permutations ns) \p ->
      signature p * productOn (\x -> a ! from x ! from (p List.!! from x)) ns
   where
    ns = finites @n
    signature p = product do
      i <- ns
      j <- List.dropWhile (<= i) ns
      pure case compare (p List.!! from i) (p List.!! from j) of
        GT -> negative one
        _ -> one
  {-# INLINE determinant #-}

permanent ::
  forall n x.
  (KnownNat n, MultiplicativeAbelian x, Ring x) =>
  M n n x -> Scalar (M n n x)
permanent (M a) = ScalarM do
  flip sumOn (List.permutations (finites @n)) \p ->
    productOn (\x -> a ! from x ! from (p List.!! from x)) (finites @n)

instance
  (KnownNat n, AdditiveAbelian x, Multiplicative x) =>
  Power (M n n x) Natural (M n n x)
  where
  (^) :: M n n x -> Natural -> M n n x
  a ^ n = case n of
    0 -> one
    _ | even n -> (a * a) ^ (n `quotient` 2)
    _ -> a * ((a * a) ^ (n `quotient` 2))
  {-# INLINE (^) #-}

rows ::
  forall m n x.
  (KnownNat m, KnownNat n) =>
  M m n x -> [V n x]
rows = toList . unM
{-# INLINE rows #-}

unrows :: forall m n x. (KnownNat m, KnownNat n) => [V n x] -> Maybe (M m n x)
unrows vs
  | count (const True) vs == natVal (Proxy @m) = Just do
      M (vn @m \i -> vs List.!! from i)
  | otherwise = Nothing
{-# INLINE unrows #-}

row :: V n x -> M 1 n x
row x = M (pure x)
{-# INLINE row #-}

columns ::
  forall m n x.
  (KnownNat m, KnownNat n) =>
  M m n x -> [V m x]
columns = rows . transpose @(M m n) @(M n m)
{-# INLINE columns #-}

uncolumns ::
  forall m n x.
  (KnownNat m, KnownNat n) =>
  [V m x] -> Maybe (M m n x)
uncolumns vs
  | count (const True) vs == n = (Just . M) do
      vn @m \i -> vn @n \j -> (vs List.!! from j) ! i
  | otherwise = Nothing
 where
  n = natVal (Proxy @n)
{-# INLINE uncolumns #-}

column :: forall m x. (KnownNat m) => V m x -> M m 1 x
column x = fromJust (uncolumns @m @1 [x])
{-# INLINE column #-}

toLists ::
  forall m n x.
  (KnownNat m, KnownNat n) =>
  M m n x -> [[x]]
toLists = morphism toList . rows
{-# INLINE toLists #-}

qr ::
  forall n x.
  (KnownNat n, Eq x, Conjugate x, Root x) =>
  M n n x -> (M n n x, M n n x)
qr a =
  let Just q = uncolumns (orthonormalize (columns a))
   in (q, transpose q * a)
{-# INLINE qr #-}

lu ::
  forall n x.
  (KnownNat n, Field x) =>
  M n n x -> (M n n x, M n n x)
lu (M a) = build 0 zero one
 where
  buildLVal !i !j (M l) (M u) =
    let go !k !s
          | k == j = s
          | otherwise = go (succ k) (s + l ! i ! k * u ! k ! j)
        s' = go zero zero
     in M do
          vn @n \i' -> vn @n \j' ->
            if i == i' && j == j' then a ! i' ! j' - s' else l ! i' ! j'
  buildL !i !j l u
    | i == natVal (Proxy @n) = l
    | otherwise = buildL (succ i) j (buildLVal i j l u) u
  buildUVal !i !j (M l) (M u) =
    let go !k !s
          | k == j = s
          | otherwise = go (succ k) (s + l ! j ! k * u ! k ! i)
        s' = go zero zero
     in M do
          vn @n \i' -> vn @n \j' ->
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
{-# INLINE lu #-}

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
                ( vn @n \i' ->
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
                ( vn @n \i' ->
                    if i == succ i'
                      then (x ! i' - coeff i' i zero z) / u ! i' ! i'
                      else z ! i'
                )
     in go n zero
{-# INLINE system #-}

minor ::
  forall n x.
  (KnownNat n, Eq x, MultiplicativeAbelian x, Ring x) =>
  Natural -> Natural -> M (n + 1) (n + 1) x -> Scalar (M n n x)
minor i_ j_ (M a) = (determinant @(M n n) . M) do
  vn @n \i -> vn @n \j ->
    a ! (if i < i_ then i else i + one) ! (if j < j_ then j else j + one)
{-# INLINE minor #-}

cofactor ::
  forall n x.
  (KnownNat n, Eq x, MultiplicativeAbelian x, Ring x, Power x Natural x) =>
  Natural -> Natural -> M (n + 1) (n + 1) x -> Scalar (M n n x)
cofactor i_ j_ a = ScalarM @n @n (negative @x one) ^ (i_ + j_) * minor i_ j_ a
{-# INLINE cofactor #-}

cofactorMatrix ::
  forall n x.
  ( KnownNat n
  , Eq x
  , MultiplicativeAbelian x
  , Ring x
  , Power x Natural x
  ) =>
  M (n + 1) (n + 1) x -> M (n + 1) (n + 1) x
cofactorMatrix a = M do
  vn @(n + 1) \i -> vn @(n + 1) \j -> (cofactor i j a).unScalar
{-# INLINE cofactorMatrix #-}

adjugate ::
  ( KnownNat n
  , Eq x
  , MultiplicativeAbelian x
  , Ring x
  , Power x Natural x
  ) =>
  M (n + 1) (n + 1) x -> M (n + 1) (n + 1) x
adjugate = transpose . cofactorMatrix
{-# INLINE adjugate #-}

characteristicPolynomial ::
  forall n x.
  (KnownNat n, Eq x, MultiplicativeAbelian x, Ring x) =>
  M n n x -> Polynomial x
characteristicPolynomial a = (determinant (tI - morphism (Polynomial.fromList . pure) a)).unScalar
 where
  tI = M (vn @n \i -> vn @n \j -> if i == j then Œ else zero)
{-# INLINE characteristicPolynomial #-}

gramMatrix ::
  forall m n x.
  (KnownNat m, KnownNat n, Ring x, Conjugate x) => V m (V n x) -> M m m x
gramMatrix vs = M do vn @m \i -> vn @m \j -> (vs ! i <•> vs ! j).unScalar

diagonal :: forall n x. (KnownNat n, Additive x, Eq x) => M n n x -> Bool
diagonal (M a) = all
  do \(i, j) -> i == j || a ! i ! j == zero
  do join (liftM2 (on (,) from)) (finites @n)
{-# INLINE diagonal #-}

upperTriangular ::
  forall n x. (KnownNat n, Additive x, Eq x) => M n n x -> Bool
upperTriangular (M a) = Data.all
  do \(i, j) -> i <= j || a ! i ! j == zero
  do join (liftM2 (on (,) from)) (finites @n)
{-# INLINE upperTriangular #-}

lowerTriangular ::
  forall n x. (KnownNat n, Additive x, Eq x) => M n n x -> Bool
lowerTriangular (M a) = Data.all
  do \(i, j) -> i >= j || a ! i ! j == zero
  do join (liftM2 (on (,) from)) (finites @n)
{-# INLINE lowerTriangular #-}

symmetric :: forall n x. (KnownNat n, Eq x) => M n n x -> Bool
symmetric (M a) = Data.all
  do \(i, j) -> a ! i ! j == a ! j ! i
  do
    i <- finites @n
    j <- List.dropWhile (<= i) finites
    pure (from i, from j)
{-# INLINE symmetric #-}

hermitian :: forall n x. (KnownNat n, Eq x, Conjugate x) => M n n x -> Bool
hermitian (M a) = Data.all
  do \(i, j) -> a ! i ! j == conjugate (a ! j ! i)
  do
    i <- finites @n
    j <- List.dropWhile (< i) finites
    pure (from i, from j)
{-# INLINE hermitian #-}

pattern M22 :: x -> x -> x -> x -> M 2 2 x
pattern M22 a b c d = M (V2 (V2 a b) (V2 c d))
{-# COMPLETE M22 #-}

pattern M23 :: x -> x -> x -> x -> x -> x -> M 2 3 x
pattern M23 a b c d e f = M (V2 (V3 a b c) (V3 d e f))
{-# COMPLETE M23 #-}

pattern M24 :: x -> x -> x -> x -> x -> x -> x -> x -> M 2 4 x
pattern M24 a b c d e f g h = M (V2 (V4 a b c d) (V4 e f g h))
{-# COMPLETE M24 #-}

pattern M32 :: x -> x -> x -> x -> x -> x -> M 3 2 x
pattern M32 a b c d e f = M (V3 (V2 a b) (V2 c d) (V2 e f))
{-# COMPLETE M32 #-}

pattern M33 :: x -> x -> x -> x -> x -> x -> x -> x -> x -> M 3 3 x
pattern M33 a b c d e f g h i = M (V3 (V3 a b c) (V3 d e f) (V3 g h i))
{-# COMPLETE M33 #-}

pattern M34 ::
  x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> M 3 4 x
pattern M34 a b c d e f g h i j k l = M (V3 (V4 a b c d) (V4 e f g h) (V4 i j k l))
{-# COMPLETE M34 #-}

pattern M42 :: x -> x -> x -> x -> x -> x -> x -> x -> M 4 2 x
pattern M42 a b c d e f g h = M (V4 (V2 a b) (V2 c d) (V2 e f) (V2 g h))
{-# COMPLETE M42 #-}

pattern M43 ::
  x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> x -> M 4 3 x
pattern M43 a b c d e f g h i j k l = M (V4 (V3 a b c) (V3 d e f) (V3 g h i) (V3 j k l))
{-# COMPLETE M43 #-}

pattern M44 ::
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
pattern M44 a b c d e f g h i j k l m n o p =
  M (V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p))
{-# COMPLETE M44 #-}
