{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Flex.Math.Matrix
  ( Matrix (transpose)
  , adjoint
  , Square (trace, determinant)
  , V (VV, V1, V2, V3, V4, V5, V6, V7, V8)
  , dimensions
  , (++)
  , vn
  , vnM
  , (!)
  , setV
  , toList
  , fromList
  , projection
  , householder
  , orthogonalize
  , orthonormalize
  , M (M, unM, M22, M23, M24, M32, M33, M34, M42, M43, M44)
  , rows
  , unrows
  , row
  , columns
  , uncolumns
  , column
  , outerproduct
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
  , Scalar (..)
  , Signature (..)
  , Term (..)
  , Laws (..)

    -- ** Re-exports
  , Nat
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
import Data.Functor.Const (Const (Const))
import Data.Kind (Type)
import Data.List qualified as List
import Data.List1 (List1)
import Data.Maybe
import Data.Ord (Ord (..), Ordering (..))
import Data.Proxy
import Data.Traversable qualified as Data
import Data.Type.Equality (type (:~:) (Refl), type (~))
import Data.Type.Ord
import GHC.Err qualified as GHC
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

type V :: Nat -> Type -> Type
data V n x where
  V1 :: !x -> V 1 x
  V2 :: !x -> !x -> V 2 x
  V3 :: !x -> !x -> !x -> V 3 x
  V4 :: !x -> !x -> !x -> !x -> V 4 x
  VV ::
    (KnownNat m, KnownNat p, m + p ~ n, 4 <= m, 4 < n, p <= 4) =>
    !(V m x) -> !(V p x) -> V n x

pattern V5 :: x -> x -> x -> x -> x -> V 5 x
pattern V5 x0 x1 x2 x3 x4 = VV (V4 x0 x1 x2 x3) (V1 x4)

pattern V6 :: x -> x -> x -> x -> x -> x -> V 6 x
pattern V6 x0 x1 x2 x3 x4 x5 = VV (V4 x0 x1 x2 x3) (V2 x4 x5)

pattern V7 :: x -> x -> x -> x -> x -> x -> x -> V 7 x
pattern V7 x0 x1 x2 x3 x4 x5 x6 = VV (V4 x0 x1 x2 x3) (V3 x4 x5 x6)

pattern V8 :: x -> x -> x -> x -> x -> x -> x -> x -> V 8 x
pattern V8 x0 x1 x2 x3 x4 x5 x6 x7 = VV (V4 x0 x1 x2 x3) (V4 x4 x5 x6 x7)

(++) :: forall m n x. (KnownNat m, KnownNat n) => V m x -> V n x -> V (m + n) x
(++) = \cases
  (V1 x) (V1 y) -> V2 x y
  (V1 x) (V2 y0 y1) -> V3 x y0 y1
  (V1 x) (V3 y0 y1 y2) -> V4 x y0 y1 y2
  (V1 x) (V4 y0 y1 y2 y3) -> VV (V4 x y0 y1 y2) (V1 y3)
  (V1 x) (VV (vm :: V m0 x) (vp :: V p0 x)) -> case (V1 x) ++ vm of
    vm' -> case sameNat (Proxy @((1 + m0) + p0)) (Proxy @(1 + n)) of
      Just Refl -> vm' ++ vp
      Nothing -> GHC.error "Flex.Math.Matrix.++: fail"
  (V2 x0 x1) (V1 y) -> V3 x0 x1 y
  (V2 x0 x1) (V2 y0 y1) -> V4 x0 x1 y0 y1
  (V2 x0 x1) (V3 y0 y1 y2) -> VV (V4 x0 x1 y0 y1) (V1 y2)
  (V2 x0 x1) (V4 y0 y1 y2 y3) -> VV (V4 x0 x1 y0 y1) (V2 y2 y3)
  (V2 x0 x1) (VV (vm :: V m0 x) (vp :: V p0 x)) -> case (V2 x0 x1) ++ vm of
    vm' -> case sameNat (Proxy @((2 + m0) + p0)) (Proxy @(2 + n)) of
      Just Refl -> vm' ++ vp
      Nothing -> GHC.error "Flex.Math.Matrix.++: fail"
  (V3 x0 x1 x2) (V1 y) -> V4 x0 x1 x2 y
  (V3 x0 x1 x2) (V2 y0 y1) -> VV (V4 x0 x1 x2 y0) (V1 y1)
  (V3 x0 x1 x2) (V3 y0 y1 y2) -> VV (V4 x0 x1 x2 y0) (V2 y1 y2)
  (V3 x0 x1 x2) (V4 y0 y1 y2 y3) -> VV (V4 x0 x1 x2 y0) (V3 y1 y2 y3)
  (V3 x0 x1 x2) (VV (vm :: V m0 x) (vp :: V p0 x)) -> case (V3 x0 x1 x2) ++ vm of
    vm' -> case sameNat (Proxy @((3 + m0) + p0)) (Proxy @(3 + n)) of
      Just Refl -> vm' ++ vp
      Nothing -> GHC.error "Flex.Math.Matrix.++: fail"
  (V4 x0 x1 x2 x3) (V1 y) -> VV (V4 x0 x1 x2 x3) (V1 y)
  (V4 x0 x1 x2 x3) (V2 y0 y1) -> VV (V4 x0 x1 x2 x3) (V2 y0 y1)
  (V4 x0 x1 x2 x3) (V3 y0 y1 y2) -> VV (V4 x0 x1 x2 x3) (V3 y0 y1 y2)
  (V4 x0 x1 x2 x3) (V4 y0 y1 y2 y3) -> VV (V4 x0 x1 x2 x3) (V4 y0 y1 y2 y3)
  (V4 x0 x1 x2 x3) (VV (vm :: V m0 x) (vp :: V p0 x)) ->
    case sameNat (Proxy @((4 + m0) + p0)) (Proxy @(4 + n)) of
      Just Refl -> case cmpNat (Proxy @4) (Proxy @(4 + m0)) of
        LTI -> case cmpNat (Proxy @4) (Proxy @(4 + n)) of
          LTI -> VV (V4 x0 x1 x2 x3 ++ vm) vp
          _ -> GHC.error "Flex.Math.Matrix.++: fail"
        _ -> GHC.error "Flex.Math.Matrix.++: fail"
      Nothing -> GHC.error "Flex.Math.Matrix.++: fail"
  (VV (vm :: V m0 x) (vp :: V p0 x)) v ->
    case sameNat (Proxy @(m0 + (p0 + n))) (Proxy @(m + n)) of
      Just Refl -> vm ++ (vp ++ v)
      Nothing -> GHC.error "Flex.Math.Matrix.++: fail"
{-# INLINE (++) #-}

instance (KnownNat n, Eq x) => Eq (V n x) where
  (==) :: V n x -> V n x -> Bool
  (==) = \cases
    (V1 x) (V1 y) -> x == y
    (V2 x0 x1) (V2 y0 y1) -> x0 == y0 && x1 == y1
    (V3 x0 x1 x2) (V3 y0 y1 y2) -> x0 == y0 && x1 == y1 && x2 == y2
    (V4 x0 x1 x2 x3) (V4 y0 y1 y2 y3) -> x0 == y0 && x1 == y1 && x2 == y2 && x3 == y3
    (VV (m0 :: V m0 x) p0) (VV (m1 :: V m1 x) p1) -> case sameNat (Proxy @m0) (Proxy @m1) of
      Just Refl -> m0 == m1 && p0 == p1
      Nothing -> (m0 ++ p0) == (m1 ++ p1)
  {-# INLINE (==) #-}
instance (KnownNat n, Ord x) => Ord (V n x) where
  compare :: (KnownNat n, Ord x) => V n x -> V n x -> Ordering
  compare = \cases
    (V1 x) (V1 y) -> compare x y
    (V2 x0 x1) (V2 y0 y1) -> compare x0 y0 <> compare x1 y1
    (V3 x0 x1 x2) (V3 y0 y1 y2) -> compare x0 y0 <> compare x1 y1 <> compare x2 y2
    (V4 x0 x1 x2 x3) (V4 y0 y1 y2 y3) -> compare x0 y0 <> compare x1 y1 <> compare x2 y2 <> compare x3 y3
    (VV (m0 :: V m0 x) (p0 :: V p0 x)) (VV (m1 :: V m1 x) (p1 :: V p1 x)) -> case sameNat (Proxy @m0) (Proxy @m1) of
      Just Refl -> compare m0 m1 <> compare p0 p1
      Nothing -> compare (m0 ++ p0) (m1 ++ p1)
  {-# INLINE compare #-}

instance (KnownNat n, Show x) => Show (V n x) where
  show :: V n x -> [Char]
  show v = "V { " <> inside v <> " }"
   where
    inside :: forall m. V m x -> [Char]
    inside = \case
      V1 x -> show x
      V2 x0 x1 -> show x0 <> " " <> show x1
      V3 x0 x1 x2 -> show x0 <> " " <> show x1 <> " " <> show x2
      V4 x0 x1 x2 x3 -> show x0 <> " " <> show x1 <> " " <> show x2 <> " " <> show x3
      VV vm vp -> inside vm <> " " <> inside vp
    {-# INLINE inside #-}
  {-# INLINE show #-}

vn :: forall n x. (KnownNat n) => (Natural -> x) -> V n x
vn f = case cmpNat (Proxy @5) (Proxy @n) of
  GTI -> case sameNat (Proxy @1) (Proxy @n) of
    Just Refl ->
        let !v0 = f 0
         in V1 v0
    Nothing -> case sameNat (Proxy @2) (Proxy @n) of
      Just Refl ->
          let !v0 = f 0
              !v1 = f 1
           in V2 v0 v1
      Nothing -> case sameNat (Proxy @3) (Proxy @n) of
        Just Refl ->
            let !v0 = f 0
                !v1 = f 1
                !v2 = f 2
             in V3 v0 v1 v2
        Nothing -> case sameNat (Proxy @4) (Proxy @n) of
          Just Refl ->
              let !v0 = f 0
                  !v1 = f 1
                  !v2 = f 2
                  !v3 = f 3
               in V4 v0 v1 v2 v3
          Nothing -> GHC.error "Flex.Math.Matrix.vn: fail"
  _ -> case sameNat (Proxy @n) (Proxy @((n - 4) + 4)) of
    Just Refl -> case cmpNat (Proxy @4) (Proxy @n) of
      LTI -> (vn f :: V (n - 4) x) ++ (vn (f . (+ natVal (Proxy @(n - 4)))) :: V 4 x)
      GTI -> GHC.error "Flex.Math.Matrix.vn: fail"
    _ -> GHC.error "Flex.Math.Matrix.vn: fail"
{-# INLINE vn #-}

vnM :: forall n x m. (KnownNat n, Apply m) => (Natural -> m x) -> m (V n x)
vnM f = case cmpNat (Proxy @5) (Proxy @n) of
  GTI -> case sameNat (Proxy @1) (Proxy @n) of
    Just Refl -> morphism V1 (f 0)
    Nothing -> case sameNat (Proxy @2) (Proxy @n) of
      Just Refl -> liftA2 V2 (f 0) (f 1)
      Nothing -> case sameNat (Proxy @3) (Proxy @n) of
        Just Refl -> liftA3 V3 (f 0) (f 1) (f 2)
        Nothing -> case sameNat (Proxy @4) (Proxy @n) of
          Just Refl -> liftA3 V4 (f 0) (f 1) (f 2) <*> (f 3)
          Nothing -> GHC.error "Flex.Math.Matrix.vnM: fail"
  _ -> case sameNat (Proxy @n) (Proxy @((n - 4) + 4)) of
    Just Refl -> case cmpNat (Proxy @4) (Proxy @n) of
      LTI ->
        liftA2
          (++)
          (vnM f :: m (V (n - 4) x))
          (vnM (f . (+ natVal (Proxy @(n - 4)))) :: m (V 4 x))
      GTI -> GHC.error "Flex.Math.Matrix.vnM: fail"
    _ -> GHC.error "Flex.Math.Matrix.vnM: fail"
{-# INLINE vnM #-}

dimensions :: forall n. (KnownNat n) => Const [Natural] n
dimensions = Const [0 .. natVal (Proxy @n) - 1]
{-# INLINE dimensions #-}

(!) :: V n x -> Natural -> x
v ! n = case v of
  V1 x -> x
  V2 x0 x1 -> case n of
    0 -> x0
    _ -> x1
  V3 x0 x1 x2 -> case n of
    0 -> x0
    1 -> x1
    _ -> x2
  V4 x0 x1 x2 x3 -> case n of
    0 -> x0
    1 -> x1
    2 -> x2
    _ -> x3
  VV (m :: V m x) (p :: V p x) -> case compare (natVal (Proxy @m)) n of
    GT -> m ! n
    _ -> p ! (n - natVal (Proxy @m))
{-# INLINE (!) #-}

setV :: Natural -> x -> V n x -> V n x
setV n x v = case v of
  V1 _ -> case n of
    0 -> V1 x
    _ -> v
  V2 x0 x1 -> case n of
    0 -> V2 x x1
    1 -> V2 x0 x
    _ -> v
  V3 x0 x1 x2 -> case n of
    0 -> V3 x x1 x2
    1 -> V3 x0 x x2
    2 -> V3 x0 x1 x
    _ -> v
  V4 x0 x1 x2 x3 -> case n of
    0 -> V4 x x1 x2 x3
    1 -> V4 x0 x x2 x3
    2 -> V4 x0 x1 x x3
    3 -> V4 x0 x1 x2 x
    _ -> v
  VV (m :: V m x) (p :: V p x) -> case compare (natVal (Proxy @m)) n of
    GT -> VV (setV n x m) p
    _ -> VV m (setV (n - natVal (Proxy @m)) x p)
{-# INLINE setV #-}

instance (KnownNat n) => Field0 (V n x) (V n x) x x where
  _0 :: Lens (V n x) (V n x) x x
  _0 = lens (! 0) (flip (setV 0))
  {-# INLINE _0 #-}
instance (KnownNat n) => Field1 (V n x) (V n x) x x where
  _1 :: Lens (V n x) (V n x) x x
  _1 = lens (! 1) (flip (setV 1))
  {-# INLINE _1 #-}
instance (KnownNat n) => Field2 (V n x) (V n x) x x where
  _2 :: Lens (V n x) (V n x) x x
  _2 = lens (! 2) (flip (setV 2))
  {-# INLINE _2 #-}
instance (KnownNat n) => Field3 (V n x) (V n x) x x where
  _3 :: Lens (V n x) (V n x) x x
  _3 = lens (! 3) (flip (setV 3))
  {-# INLINE _3 #-}
instance (KnownNat n) => Field4 (V n x) (V n x) x x where
  _4 :: Lens (V n x) (V n x) x x
  _4 = lens (! 4) (flip (setV 4))
  {-# INLINE _4 #-}
instance (KnownNat n) => Field5 (V n x) (V n x) x x where
  _5 :: Lens (V n x) (V n x) x x
  _5 = lens (! 5) (flip (setV 5))
  {-# INLINE _5 #-}
instance (KnownNat n) => Field6 (V n x) (V n x) x x where
  _6 :: Lens (V n x) (V n x) x x
  _6 = lens (! 6) (flip (setV 6))
  {-# INLINE _6 #-}
instance (KnownNat n) => Field7 (V n x) (V n x) x x where
  _7 :: Lens (V n x) (V n x) x x
  _7 = lens (! 7) (flip (setV 7))
  {-# INLINE _7 #-}

instance Morphisms (->) (->) (V n) where
  morphism :: (x -> y) -> V n x -> V n y
  morphism x_y = \case
    V1 x -> V1 (x_y x)
    V2 x0 x1 -> V2 (x_y x0) (x_y x1)
    V3 x0 x1 x2 -> V3 (x_y x0) (x_y x1) (x_y x2)
    V4 x0 x1 x2 x3 -> V4 (x_y x0) (x_y x1) (x_y x2) (x_y x3)
    VV m p -> VV (morphism x_y m) (morphism x_y p)
  {-# INLINE morphism #-}
instance Data.Functor (V n) where
  fmap :: (a -> b) -> V n a -> V n b
  fmap = morphism
  {-# INLINE fmap #-}
instance Folds (->) (->) (V n) where
  foldWith :: (Monoid z) => (x -> z) -> V n x -> z
  foldWith x_z = \case
    V1 x -> x_z x
    V2 x0 x1 -> x_z x0 <> x_z x1
    V3 x0 x1 x2 -> x_z x0 <> x_z x1 <> x_z x2
    V4 x0 x1 x2 x3 -> x_z x0 <> x_z x1 <> x_z x2 <> x_z x3
    VV m p -> foldWith x_z m <> foldWith x_z p
  {-# INLINE foldWith #-}
instance Data.Foldable (V n) where
  foldMap :: (Monoid m) => (a -> m) -> V n a -> m
  foldMap = foldWith
  {-# INLINE foldMap #-}
instance Folds1 (->) (->) (V n) where
  foldWith1 :: (Semigroup z) => (x -> z) -> V n x -> z
  foldWith1 x_z = \case
    V1 x -> x_z x
    V2 x0 x1 -> x_z x0 <> x_z x1
    V3 x0 x1 x2 -> x_z x0 <> x_z x1 <> x_z x2
    V4 x0 x1 x2 x3 -> x_z x0 <> x_z x1 <> x_z x2 <> x_z x3
    VV m p -> foldWith1 x_z m <> foldWith1 x_z p
  {-# INLINE foldWith1 #-}
instance Traversals (->) (->) (V n) where
  traverse :: (Applicative g) => (x -> g y) -> V n x -> g (V n y)
  traverse x_gy = \case
    V1 x -> morphism V1 (x_gy x)
    V2 x0 x1 -> liftA2 V2 (x_gy x0) (x_gy x1)
    V3 x0 x1 x2 -> liftA3 V3 (x_gy x0) (x_gy x1) (x_gy x2)
    V4 x0 x1 x2 x3 -> liftA3 V4 (x_gy x0) (x_gy x1) (x_gy x2) <*> (x_gy x3)
    VV m p -> liftA2 VV (traverse x_gy m) (traverse x_gy p)
  {-# INLINE traverse #-}
instance Data.Traversable (V n) where
  traverse :: (Control.Applicative g) => (x -> g y) -> V n x -> g (V n y)
  traverse x_gy = \case
    V1 x -> Data.fmap V1 (x_gy x)
    V2 x0 x1 -> Control.liftA2 V2 (x_gy x0) (x_gy x1)
    V3 x0 x1 x2 -> Control.liftA3 V3 (x_gy x0) (x_gy x1) (x_gy x2)
    V4 x0 x1 x2 x3 -> Control.liftA3 V4 (x_gy x0) (x_gy x1) (x_gy x2) Control.<*> (x_gy x3)
    VV m p -> Control.liftA2 VV (Data.traverse x_gy m) (Data.traverse x_gy p)
  {-# INLINE traverse #-}
instance Traversals1 (->) (->) (V n) where
  traverse1 :: (Apply g) => (x -> g y) -> V n x -> g (V n y)
  traverse1 x_gy = \case
    V1 x -> morphism V1 (x_gy x)
    V2 x0 x1 -> liftA2 V2 (x_gy x0) (x_gy x1)
    V3 x0 x1 x2 -> liftA3 V3 (x_gy x0) (x_gy x1) (x_gy x2)
    V4 x0 x1 x2 x3 -> liftA3 V4 (x_gy x0) (x_gy x1) (x_gy x2) <*> (x_gy x3)
    VV m p -> liftA2 VV (traverse1 x_gy m) (traverse1 x_gy p)
  {-# INLINE traverse1 #-}

instance Morphisms (Ix Natural) (->) (V n) where
  morphism :: Ix Natural x y -> V n x -> V n y
  morphism (Ix i_x_y) = \case
    V1 x -> V1 (i_x_y 0 x)
    V2 x0 x1 -> V2 (i_x_y 0 x0) (i_x_y 1 x1)
    V3 x0 x1 x2 -> V3 (i_x_y 0 x0) (i_x_y 1 x1) (i_x_y 2 x2)
    V4 x0 x1 x2 x3 -> V4 (i_x_y 0 x0) (i_x_y 1 x1) (i_x_y 2 x2) (i_x_y 3 x3)
    VV (m :: V m x) p ->
      VV
        (morphism (Ix i_x_y) m)
        (morphism (Ix (i_x_y . (+ natVal (Proxy @m)))) p)
  {-# INLINE morphism #-}
instance Morphisms (Ix Integer) (->) (V n) where
  morphism :: Ix Integer x y -> V n x -> V n y
  morphism (Ix i_x_y) = \case
    V1 x -> V1 (i_x_y 0 x)
    V2 x0 x1 -> V2 (i_x_y 0 x0) (i_x_y 1 x1)
    V3 x0 x1 x2 -> V3 (i_x_y 0 x0) (i_x_y 1 x1) (i_x_y 2 x2)
    V4 x0 x1 x2 x3 -> V4 (i_x_y 0 x0) (i_x_y 1 x1) (i_x_y 2 x2) (i_x_y 3 x3)
    VV (m :: V m x) p ->
      VV
        (morphism (Ix i_x_y) m)
        (morphism (Ix (i_x_y . (+ from (natVal (Proxy @m))))) p)
  {-# INLINE morphism #-}
instance Morphisms (Ix Int) (->) (V n) where
  morphism :: Ix Int x y -> V n x -> V n y
  morphism (Ix i_x_y) = \case
    V1 x -> V1 (i_x_y 0 x)
    V2 x0 x1 -> V2 (i_x_y 0 x0) (i_x_y 1 x1)
    V3 x0 x1 x2 -> V3 (i_x_y 0 x0) (i_x_y 1 x1) (i_x_y 2 x2)
    V4 x0 x1 x2 x3 -> V4 (i_x_y 0 x0) (i_x_y 1 x1) (i_x_y 2 x2) (i_x_y 3 x3)
    VV (m :: V m x) p ->
      VV
        (morphism (Ix i_x_y) m)
        (morphism (Ix (i_x_y . (+ from (natVal (Proxy @m))))) p)
  {-# INLINE morphism #-}
instance Folds (Ix Natural) (->) (V n) where
  foldWith :: (Monoid z) => Ix Natural x z -> V n x -> z
  foldWith (Ix i_x_z) = \case
    V1 x -> i_x_z 0 x
    V2 x0 x1 -> i_x_z 0 x0 <> i_x_z 1 x1
    V3 x0 x1 x2 -> i_x_z 0 x0 <> i_x_z 1 x1 <> i_x_z 2 x2
    V4 x0 x1 x2 x3 -> i_x_z 0 x0 <> i_x_z 1 x1 <> i_x_z 2 x2 <> i_x_z 3 x3
    VV (m :: V m x) p ->
      foldWith (Ix i_x_z) m
        <> foldWith (Ix (i_x_z . (+ natVal (Proxy @m)))) p
  {-# INLINE foldWith #-}
instance Folds (Ix Integer) (->) (V n) where
  foldWith :: (Monoid z) => Ix Integer x z -> V n x -> z
  foldWith (Ix i_x_z) = \case
    V1 x -> i_x_z 0 x
    V2 x0 x1 -> i_x_z 0 x0 <> i_x_z 1 x1
    V3 x0 x1 x2 -> i_x_z 0 x0 <> i_x_z 1 x1 <> i_x_z 2 x2
    V4 x0 x1 x2 x3 -> i_x_z 0 x0 <> i_x_z 1 x1 <> i_x_z 2 x2 <> i_x_z 3 x3
    VV (m :: V m x) p ->
      foldWith (Ix i_x_z) m
        <> foldWith (Ix (i_x_z . (+ from (natVal (Proxy @m))))) p
  {-# INLINE foldWith #-}
instance Folds (Ix Int) (->) (V n) where
  foldWith :: (Monoid z) => Ix Int x z -> V n x -> z
  foldWith (Ix i_x_z) = \case
    V1 x -> i_x_z 0 x
    V2 x0 x1 -> i_x_z 0 x0 <> i_x_z 1 x1
    V3 x0 x1 x2 -> i_x_z 0 x0 <> i_x_z 1 x1 <> i_x_z 2 x2
    V4 x0 x1 x2 x3 -> i_x_z 0 x0 <> i_x_z 1 x1 <> i_x_z 2 x2 <> i_x_z 3 x3
    VV (m :: V m x) p ->
      foldWith (Ix i_x_z) m
        <> foldWith (Ix (i_x_z . (+ from (natVal (Proxy @m))))) p
  {-# INLINE foldWith #-}
instance Folds1 (Ix Natural) (->) (V n) where
  foldWith1 :: (Semigroup z) => Ix Natural x z -> V n x -> z
  foldWith1 (Ix i_x_z) = \case
    V1 x -> i_x_z 0 x
    V2 x0 x1 -> i_x_z 0 x0 <> i_x_z 1 x1
    V3 x0 x1 x2 -> i_x_z 0 x0 <> i_x_z 1 x1 <> i_x_z 2 x2
    V4 x0 x1 x2 x3 -> i_x_z 0 x0 <> i_x_z 1 x1 <> i_x_z 2 x2 <> i_x_z 3 x3
    VV (m :: V m x) p ->
      foldWith1 (Ix i_x_z) m
        <> foldWith1 (Ix (i_x_z . (+ natVal (Proxy @m)))) p
  {-# INLINE foldWith1 #-}
instance Folds1 (Ix Integer) (->) (V n) where
  foldWith1 :: (Semigroup z) => Ix Integer x z -> V n x -> z
  foldWith1 (Ix i_x_z) = \case
    V1 x -> i_x_z 0 x
    V2 x0 x1 -> i_x_z 0 x0 <> i_x_z 1 x1
    V3 x0 x1 x2 -> i_x_z 0 x0 <> i_x_z 1 x1 <> i_x_z 2 x2
    V4 x0 x1 x2 x3 -> i_x_z 0 x0 <> i_x_z 1 x1 <> i_x_z 2 x2 <> i_x_z 3 x3
    VV (m :: V m x) p ->
      foldWith1 (Ix i_x_z) m
        <> foldWith1 (Ix (i_x_z . (+ from (natVal (Proxy @m))))) p
  {-# INLINE foldWith1 #-}
instance Folds1 (Ix Int) (->) (V n) where
  foldWith1 :: (Semigroup z) => Ix Int x z -> V n x -> z
  foldWith1 (Ix i_x_z) = \case
    V1 x -> i_x_z 0 x
    V2 x0 x1 -> i_x_z 0 x0 <> i_x_z 1 x1
    V3 x0 x1 x2 -> i_x_z 0 x0 <> i_x_z 1 x1 <> i_x_z 2 x2
    V4 x0 x1 x2 x3 -> i_x_z 0 x0 <> i_x_z 1 x1 <> i_x_z 2 x2 <> i_x_z 3 x3
    VV (m :: V m x) p ->
      foldWith1 (Ix i_x_z) m
        <> foldWith1 (Ix (i_x_z . (+ from (natVal (Proxy @m))))) p
  {-# INLINE foldWith1 #-}
instance Traversals (Ix Natural) (->) (V n) where
  traverse ::
    (Applicative g) => Ix Natural x (g y) -> V n x -> g (V n y)
  traverse (Ix i_x_gy) = \case
    V1 x -> morphism V1 (i_x_gy 0 x)
    V2 x0 x1 -> liftA2 V2 (i_x_gy 0 x0) (i_x_gy 1 x1)
    V3 x0 x1 x2 -> liftA3 V3 (i_x_gy 0 x0) (i_x_gy 1 x1) (i_x_gy 2 x2)
    V4 x0 x1 x2 x3 -> liftA3 V4 (i_x_gy 0 x0) (i_x_gy 1 x1) (i_x_gy 2 x2) <*> (i_x_gy 3 x3)
    VV (m :: V m x) p ->
      liftA2
        VV
        (traverse (Ix i_x_gy) m)
        (traverse (Ix (i_x_gy . (+ natVal (Proxy @m)))) p)
  {-# INLINE traverse #-}
instance Traversals (Ix Integer) (->) (V n) where
  traverse ::
    (Applicative g) => Ix Integer x (g y) -> V n x -> g (V n y)
  traverse (Ix i_x_gy) = \case
    V1 x -> morphism V1 (i_x_gy 0 x)
    V2 x0 x1 -> liftA2 V2 (i_x_gy 0 x0) (i_x_gy 1 x1)
    V3 x0 x1 x2 -> liftA3 V3 (i_x_gy 0 x0) (i_x_gy 1 x1) (i_x_gy 2 x2)
    V4 x0 x1 x2 x3 -> liftA3 V4 (i_x_gy 0 x0) (i_x_gy 1 x1) (i_x_gy 2 x2) <*> (i_x_gy 3 x3)
    VV (m :: V m x) p ->
      liftA2
        VV
        (traverse (Ix i_x_gy) m)
        (traverse (Ix (i_x_gy . (+ from (natVal (Proxy @m))))) p)
  {-# INLINE traverse #-}
instance Traversals (Ix Int) (->) (V n) where
  traverse ::
    (Applicative g) => Ix Int x (g y) -> V n x -> g (V n y)
  traverse (Ix i_x_gy) = \case
    V1 x -> morphism V1 (i_x_gy 0 x)
    V2 x0 x1 -> liftA2 V2 (i_x_gy 0 x0) (i_x_gy 1 x1)
    V3 x0 x1 x2 -> liftA3 V3 (i_x_gy 0 x0) (i_x_gy 1 x1) (i_x_gy 2 x2)
    V4 x0 x1 x2 x3 -> liftA3 V4 (i_x_gy 0 x0) (i_x_gy 1 x1) (i_x_gy 2 x2) <*> (i_x_gy 3 x3)
    VV (m :: V m x) p ->
      liftA2
        VV
        (traverse (Ix i_x_gy) m)
        (traverse (Ix (i_x_gy . (+ from (natVal (Proxy @m))))) p)
  {-# INLINE traverse #-}
instance Traversals1 (Ix Natural) (->) (V n) where
  traverse1 ::
    (Apply g) => Ix Natural x (g y) -> V n x -> g (V n y)
  traverse1 (Ix i_x_gy) = \case
    V1 x -> morphism V1 (i_x_gy 0 x)
    V2 x0 x1 -> liftA2 V2 (i_x_gy 0 x0) (i_x_gy 1 x1)
    V3 x0 x1 x2 -> liftA3 V3 (i_x_gy 0 x0) (i_x_gy 1 x1) (i_x_gy 2 x2)
    V4 x0 x1 x2 x3 -> liftA3 V4 (i_x_gy 0 x0) (i_x_gy 1 x1) (i_x_gy 2 x2) <*> (i_x_gy 3 x3)
    VV (m :: V m x) p ->
      liftA2
        VV
        (traverse1 (Ix i_x_gy) m)
        (traverse1 (Ix (i_x_gy . (+ natVal (Proxy @m)))) p)
  {-# INLINE traverse1 #-}
instance Traversals1 (Ix Integer) (->) (V n) where
  traverse1 ::
    (Apply g) => Ix Integer x (g y) -> V n x -> g (V n y)
  traverse1 (Ix i_x_gy) = \case
    V1 x -> morphism V1 (i_x_gy 0 x)
    V2 x0 x1 -> liftA2 V2 (i_x_gy 0 x0) (i_x_gy 1 x1)
    V3 x0 x1 x2 -> liftA3 V3 (i_x_gy 0 x0) (i_x_gy 1 x1) (i_x_gy 2 x2)
    V4 x0 x1 x2 x3 -> liftA3 V4 (i_x_gy 0 x0) (i_x_gy 1 x1) (i_x_gy 2 x2) <*> (i_x_gy 3 x3)
    VV (m :: V m x) p ->
      liftA2
        VV
        (traverse1 (Ix i_x_gy) m)
        (traverse1 (Ix (i_x_gy . (+ from (natVal (Proxy @m))))) p)
  {-# INLINE traverse1 #-}
instance Traversals1 (Ix Int) (->) (V n) where
  traverse1 ::
    (Apply g) => Ix Int x (g y) -> V n x -> g (V n y)
  traverse1 (Ix i_x_gy) = \case
    V1 x -> morphism V1 (i_x_gy 0 x)
    V2 x0 x1 -> liftA2 V2 (i_x_gy 0 x0) (i_x_gy 1 x1)
    V3 x0 x1 x2 -> liftA3 V3 (i_x_gy 0 x0) (i_x_gy 1 x1) (i_x_gy 2 x2)
    V4 x0 x1 x2 x3 -> liftA3 V4 (i_x_gy 0 x0) (i_x_gy 1 x1) (i_x_gy 2 x2) <*> (i_x_gy 3 x3)
    VV (m :: V m x) p ->
      liftA2
        VV
        (traverse1 (Ix i_x_gy) m)
        (traverse1 (Ix (i_x_gy . (+ from (natVal (Proxy @m))))) p)
  {-# INLINE traverse1 #-}
instance (KnownNat n) => Pure (V n) where
  pure :: forall x. x -> V n x
  pure x = case cmpNat (Proxy @5) (Proxy @n) of
    GTI -> case sameNat (Proxy @1) (Proxy @n) of
      Just Refl -> V1 x
      _ -> case sameNat (Proxy @2) (Proxy @n) of
        Just Refl -> V2 x x
        _ -> case sameNat (Proxy @3) (Proxy @n) of
          Just Refl -> V3 x x x
          _ -> case sameNat (Proxy @4) (Proxy @n) of
            Just Refl -> V4 x x x x
            _ -> GHC.error "pure: V 0"
    _ -> case sameNat (Proxy @n) (Proxy @((n - 4) + 4)) of
      Just Refl -> case cmpNat (Proxy @4) (Proxy @n) of
        LTI -> (pure x :: V (n - 4) x) ++ (pure x :: V 4 x)
        GTI -> GHC.error "pure: fail"
      _ -> GHC.error "pure: fail"
instance Apply (V n) where
  liftA2 :: forall x y z. (x -> y -> z) -> V n x -> V n y -> V n z
  liftA2 x_y_z = \cases
    (V1 x) (V1 y) -> V1 (x_y_z x y)
    (V2 x0 x1) (V2 y0 y1) -> V2 (x_y_z x0 y0) (x_y_z x1 y1)
    (V3 x0 x1 x2) (V3 y0 y1 y2) -> V3 (x_y_z x0 y0) (x_y_z x1 y1) (x_y_z x2 y2)
    (V4 x0 x1 x2 x3) (V4 y0 y1 y2 y3) -> V4 (x_y_z x0 y0) (x_y_z x1 y1) (x_y_z x2 y2) (x_y_z x3 y3)
    (VV (m0 :: V m0 x) p0) (VV (m1 :: V m1 y) p1) ->
      case sameNat (Proxy @m0) (Proxy @m1) of
        Just Refl -> VV (liftA2 x_y_z m0 m1) (liftA2 x_y_z p0 p1)
        Nothing -> liftA2 x_y_z (m0 ++ p0) (m1 ++ p1)
  {-# INLINE liftA2 #-}
instance (KnownNat n) => Control.Applicative (V n) where
  pure :: x -> V n x
  pure = pure
  {-# INLINE pure #-}
  (<*>) :: V n (x -> y) -> V n x -> V n y
  (<*>) = liftA2 ($)
  {-# INLINE (<*>) #-}

toList :: V n x -> [x]
toList = \case
  V1 x -> [x]
  V2 x0 x1 -> [x0, x1]
  V3 x0 x1 x2 -> [x0, x1, x2]
  V4 x0 x1 x2 x3 -> [x0, x1, x2, x3]
  VV m p -> toList m <> toList p
{-# INLINE toList #-}

fromList :: forall n x. (KnownNat n) => [x] -> Maybe (V n x)
fromList xs = case sameNat (Proxy @1) (Proxy @n) of
  Just Refl -> case xs of
    [x] -> Just (V1 x)
    _ -> Nothing
  _ -> case sameNat (Proxy @2) (Proxy @n) of
    Just Refl -> case xs of
      [x0, x1] -> Just (V2 x0 x1)
      _ -> Nothing
    _ -> case sameNat (Proxy @3) (Proxy @n) of
      Just Refl -> case xs of
        [x0, x1, x2] -> Just (V3 x0 x1 x2)
        _ -> Nothing
      _ -> case sameNat (Proxy @4) (Proxy @n) of
        Just Refl -> case xs of
          [x0, x1, x2, x3] -> Just (V4 x0 x1 x2 x3)
          _ -> Nothing
        _ -> case cmpNat (Proxy @4) (Proxy @n) of
          LTI ->
            let (m, p) = List.splitAt (from (natVal (Proxy @n)) - 4) xs
             in case (fromList @(n - 4) m, fromList @4 p) of
                  (Just vm, Just vp) -> case sameNat (Proxy @((n - 4) + 4)) (Proxy @n) of
                    Just Refl -> Just (vm ++ vp)
                    Nothing -> Nothing
                  _ -> Nothing
          _ -> Nothing
{-# INLINE fromList #-}

instance Each (V n x) (V n x) x x
instance Indices (V n x) where
  type Index (V n x) = Natural
  type Value (V n x) = x
  index :: Natural -> Traversal' (V n x) x
  index i = lens (! i) (flip (setV i))
  {-# INLINE index #-}

instance (KnownNat n) => Collectable (V n) where
  distribute :: (Along f) => f (V n x) -> V n (f x)
  distribute fv = vn @n \i -> morphism (! i) fv
  {-# INLINE distribute #-}
instance (KnownNat n) => Tabulation (V n) where
  type Table (V n) = Finite n
  fromTable :: (Table (V n) -> x) -> V n x
  fromTable tx = vn @n \i -> tx (finite (from i))
  {-# INLINE fromTable #-}
  toTable :: V n x -> Table (V n) -> x
  toTable v i = v ! from (getFinite i)
  {-# INLINE toTable #-}

instance (Addition x x x) => Addition (V n x) (V n x) (V n x) where
  (+.) :: V n x -> V n x -> V n x
  u +. v = liftA2 (+) u v
  {-# INLINE (+.) #-}
instance (KnownNat n, Additive x) => Additive (V n x) where
  zero :: V n x
  zero = pure zero
  {-# INLINE zero #-}
instance (KnownNat n, AdditiveAbelian x) => AdditiveAbelian (V n x)
instance (Subtraction x x x) => Subtraction (V n x) (V n x) (V n x) where
  (-.) :: V n x -> V n x -> V n x
  u -. v = liftA2 (-) u v
  {-# INLINE (-.) #-}
instance (KnownNat n, AdditiveGroup x) => AdditiveGroup (V n x) where
  negative :: V n x -> V n x
  negative = morphism negative
  {-# INLINE negative #-}

instance (Multiplication x x x) => Multiplication x (V n x) (V n x) where
  (*.) :: x -> V n x -> V n x
  x *. v = morphism (x *) v
  {-# INLINE (*.) #-}
instance (Multiplication x x x) => Multiplication (V n x) x (V n x) where
  (*.) :: V n x -> x -> V n x
  v *. x = morphism (* x) v
  {-# INLINE (*.) #-}
instance (Division x x x) => Division (V n x) x (V n x) where
  (/.) :: V n x -> x -> V n x
  v /. x = morphism (/. x) v
  {-# INLINE (/.) #-}

instance (KnownNat n, From y x) => From y (Scalar (V n x)) where
  from :: y -> Scalar (V n x)
  from n = ScalarV (from n)
  {-# INLINE from #-}

instance
  (KnownNat n, Addition x x x) =>
  Addition (Scalar (V n x)) (Scalar (V n x)) (Scalar (V n x))
  where
  (+.) :: Scalar (V n x) -> Scalar (V n x) -> Scalar (V n x)
  ScalarV x +. ScalarV y = ScalarV (x + y)
  {-# INLINE (+.) #-}
instance
  (KnownNat n, Subtraction x x x) =>
  Subtraction (Scalar (V n x)) (Scalar (V n x)) (Scalar (V n x))
  where
  (-.) :: Scalar (V n x) -> Scalar (V n x) -> Scalar (V n x)
  ScalarV x -. ScalarV y = ScalarV (x - y)
  {-# INLINE (-.) #-}
instance
  (KnownNat n, Multiplication x x x) =>
  Multiplication (Scalar (V n x)) (Scalar (V n x)) (Scalar (V n x))
  where
  (*.) :: Scalar (V n x) -> Scalar (V n x) -> Scalar (V n x)
  ScalarV x *. ScalarV y = ScalarV (x * y)
  {-# INLINE (*.) #-}
instance
  (KnownNat n, Division x x x) =>
  Division (Scalar (V n x)) (Scalar (V n x)) (Scalar (V n x))
  where
  (/.) :: Scalar (V n x) -> Scalar (V n x) -> Scalar (V n x)
  ScalarV x /. ScalarV y = ScalarV (x / y)
  {-# INLINE (/.) #-}
instance
  (KnownNat n, Multiplication x x x) =>
  Multiplication (Scalar (V n x)) (V n x) (V n x)
  where
  (*.) :: Scalar (V n x) -> V n x -> V n x
  ScalarV x *. v = x *. v
  {-# INLINE (*.) #-}
instance
  (KnownNat n, Multiplication x x x) =>
  Multiplication (V n x) (Scalar (V n x)) (V n x)
  where
  (*.) :: V n x -> Scalar (V n x) -> V n x
  v *. ScalarV x = v *. x
  {-# INLINE (*.) #-}
instance (KnownNat n, Division x x x) => Division (V n x) (Scalar (V n x)) (V n x) where
  (/.) :: V n x -> Scalar (V n x) -> V n x
  v /. ScalarV x = v /. x
  {-# INLINE (/.) #-}
instance
  (KnownNat n, Power x Rational x) =>
  Power (Scalar (V n x)) Rational (Scalar (V n x))
  where
  (^) :: Scalar (V n x) -> Rational -> Scalar (V n x)
  ScalarV x ^ r = ScalarV (x ^ r)
  {-# INLINE (^) #-}
instance (KnownNat n, Absolute x x) => Absolute (Scalar (V n x)) x where
  absolute :: Scalar (V n x) -> x
  absolute (ScalarV k) = absolute k
  {-# INLINE absolute #-}
instance (KnownNat n, Absolute x x) => Absolute (Scalar (V n x)) (Scalar (V n x)) where
  absolute :: Scalar (V n x) -> Scalar (V n x)
  absolute (ScalarV k) = ScalarV (absolute k)
  {-# INLINE absolute #-}

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
  u • v = ScalarV (sum (liftA2 (*) u v))
  {-# INLINE (•) #-}
instance (KnownNat n, Ring x, Conjugate x) => Sesquilinear (V n x) where
  (<•>) :: V n x -> V n x -> Scalar (V n x)
  u <•> v = ScalarV (sum (liftA2 (\_u _v -> _u * conjugate _v) u v))
  {-# INLINE (<•>) #-}
instance (KnownNat n, Ring x, Conjugate x) => InnerProduct (V n x)

projection ::
  forall n x. (KnownNat n, Field x, Conjugate x) => V n x -> V n x -> V n x
projection u v = ((u <•> v) / quadrance u) *. u
{-# INLINE projection #-}

householder ::
  forall n x. (KnownNat n, Field x, Conjugate x) => V n x -> V n x -> V n x
householder q x = x - (one + one) * ((q <•> x) / quadrance q) *. q
{-# INLINE householder #-}

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
{-# INLINE orthogonalize #-}

orthonormalize ::
  (KnownNat n, Field x, Conjugate x, Root x) =>
  [V n x] -> [V n x]
orthonormalize = morphism normalize . orthogonalize
{-# INLINE orthonormalize #-}

newtype M m n x = M {unM :: V m (V n x)} deriving (Data.Functor)
instance Morphisms (->) (->) (M m n) where
  morphism :: forall x y. (x -> y) -> M m n x -> M m n y
  morphism x_y (M a) = M (morphism (morphism x_y :: V n x -> V n y) a)
  {-# INLINE morphism #-}
instance Morphisms (Ix (Natural, Natural)) (->) (M m n) where
  morphism :: forall x y. Ix (Natural, Natural) x y -> M m n x -> M m n y
  morphism (Ix i_x_y) (M a) =
    M (morphism (Ix \i -> morphism (Ix \j -> i_x_y (i, j))) a)
  {-# INLINE morphism #-}
instance (KnownNat m, KnownNat n) => Folds (->) (->) (M m n) where
  foldWith :: forall x z. (Monoid z) => (x -> z) -> M m n x -> z
  foldWith x_z (M a) = foldWith (foldWith x_z :: V n x -> z) a
  {-# INLINE foldWith #-}
instance (KnownNat m, KnownNat n) => Folds (Ix (Natural, Natural)) (->) (M m n) where
  foldWith :: (Monoid z) => Ix (Natural, Natural) x z -> M m n x -> z
  foldWith (Ix ij_x_z) (M vs) =
    foldWith (Ix \i -> foldWith (Ix \j -> ij_x_z (i, j))) vs
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
    morphism M (traverse (Ix \i -> traverse (Ix \j -> ij_x_z (i, j))) a)
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
  show :: M m n x -> [Char]
  show (M a) = "M " <> inside a
   where
    inside :: forall k p. (KnownNat k, KnownNat p, Show x) => V k (V p x) -> [Char]
    inside = \case
      V1 x -> "{ " <> inside2 x <> " }"
      V2 x0 x1 -> "{ " <> inside2 x0 <> " " <> inside2 x1 <> " }"
      V3 x0 x1 x2 -> "{ " <> inside2 x0 <> " " <> inside2 x1 <> " " <> inside2 x2 <> " }"
      V4 x0 x1 x2 x3 ->
        "{ "
          <> inside2 x0
          <> " "
          <> inside2 x1
          <> " "
          <> inside2 x2
          <> " "
          <> inside2 x3
          <> " }"
      VV m p -> "{ " <> foldWith ((<> " ") . inside2) m <> foldWith ((<> " ") . inside2) p <> "}"
    {-# INLINE inside #-}
    inside2 :: forall p. (KnownNat p, Show x) => V p x -> [Char]
    inside2 = \case
      V1 x -> "{ " <> show x <> " }"
      V2 x0 x1 -> "{ " <> show x0 <> " " <> show x1 <> " }"
      V3 x0 x1 x2 -> "{ " <> show x0 <> " " <> show x1 <> " " <> show x2 <> " }"
      V4 x0 x1 x2 x3 ->
        "{ " <> show x0 <> " " <> show x1 <> " " <> show x2 <> " " <> show x3 <> " }"
      VV m p -> "{ " <> foldWith ((<> " ") . show) m <> foldWith ((<> " ") . show) p <> "}"
    {-# INLINE inside2 #-}
  {-# INLINE show #-}

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
  one = M do
    vn @n \i -> vn @n \j -> if i == j then one else zero
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

instance (KnownNat m, KnownNat n) => Indices (M m n x) where
  type Index (M m n x) = (Natural, Natural)
  type Value (M m n x) = x
  index :: (Natural, Natural) -> Traversal' (M m n x) x
  index (i, j) x_fx' v@(M vs)
    | from i < n = case vs ! i ! j of
        x -> morphism (\x' -> M ((index i . index j .~ x') vs)) (x_fx' x)
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
  transpose (M a) = M $ vn @n \i -> vn @m \j -> a ! j ! i
  {-# INLINE transpose #-}
instance (KnownNat n, Eq x, MultiplicativeAbelian x, Ring x) => Square (M n n) x where
  trace :: M n n x -> Scalar (M n n x)
  trace (M a) = ScalarM do
    sumOn (\k -> a ! k ! k) (dimensions @n).getConst
  {-# INLINE trace #-}
  determinant :: M n n x -> Scalar (M n n x)
  determinant (M a) = ScalarM do
    flip
      sumOn
      (List.permutations (dimensions @n).getConst)
      \p ->
        signature p
          * productOn
            (\x -> a ! x ! (p List.!! from x))
            (dimensions @n).getConst
   where
    signature p = (\cnt -> if even cnt then one else negative one) $ count id do
      x <- (dimensions @n).getConst
      y <- List.dropWhile (<= x) (dimensions @n).getConst
      pure $ (p List.!! from x) > (p List.!! from y)
  {-# INLINE determinant #-}

instance
  (KnownNat n, AdditiveAbelian x, Multiplicative x) =>
  Power (M n n x) Natural (M n n x)
  where
  (^) :: M n n x -> Natural -> M n n x
  a ^ n = case n of
    0 -> one
    _ -> a * a ^ pred n
  {-# INLINE (^) #-}

rows ::
  forall m n x.
  (KnownNat m, KnownNat n) =>
  M m n x -> [V n x]
rows (M a) = morphism (a !) (dimensions @m).getConst
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
columns a =
  let M aT = transpose @(M m n) @(M n m) a
   in morphism (aT !) (dimensions @n).getConst
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
  let q = fromJust . uncolumns $ orthonormalize (columns a)
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
     in M $ vn @n \i' -> vn @n \j' ->
          if i == i' && j == j' then a ! i' ! j' - s' else l ! i' ! j'
  buildL !i !j l u
    | i == natVal (Proxy @n) = l
    | otherwise = buildL (succ i) j (buildLVal i j l u) u
  buildUVal !i !j (M l) (M u) =
    let go !k !s
          | k == j = s
          | otherwise = go (succ k) (s + l ! j ! k * u ! k ! i)
        s' = go zero zero
     in M $ vn @n \i' -> vn @n \j' ->
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
                      then (x ! i' - coeff i' (succ i') zero z) / u ! i' ! i'
                      else z ! i'
                )
     in go n zero
{-# INLINE system #-}

minor ::
  forall n x.
  (KnownNat n, Eq x, MultiplicativeAbelian x, Ring x) =>
  Natural -> Natural -> M (n + 1) (n + 1) x -> Scalar (M n n x)
minor i_ j_ (M a) = determinant @(M n n) $ M do
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
  , KnownNat (n + 1)
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
  M n n x -> List1 (Scalar (M n n x))
characteristicPolynomial a = morphism ScalarM (determinant (tI - morphism pure a)).unScalar
 where
  tI = M $ vn @n \i -> vn @n \j -> if i == j then variable else zero
{-# INLINE characteristicPolynomial #-}

diagonal :: forall n x. (KnownNat n, Additive x, Eq x) => M n n x -> Bool
diagonal (M a) = all
  do \(i, j) -> i == j || a ! i ! j == zero
  do join (liftM2 (,)) (dimensions @n).getConst
{-# INLINE diagonal #-}

upperTriangular ::
  forall n x. (KnownNat n, Additive x, Eq x) => M n n x -> Bool
upperTriangular (M a) = Data.all
  do \(i, j) -> i <= j || a ! i ! j == zero
  do join (liftM2 (,)) (dimensions @n).getConst
{-# INLINE upperTriangular #-}

lowerTriangular ::
  forall n x. (KnownNat n, Additive x, Eq x) => M n n x -> Bool
lowerTriangular (M a) = Data.all
  do \(i, j) -> i >= j || a ! i ! j == zero
  do join (liftM2 (,)) (dimensions @n).getConst
{-# INLINE lowerTriangular #-}

symmetric :: forall n x. (KnownNat n, Eq x) => M n n x -> Bool
symmetric (M a) = Data.all
  do \(i, j) -> a ! i ! j == a ! j ! i
  do join (liftM2 (,)) (dimensions @n).getConst
{-# INLINE symmetric #-}

hermitian :: forall n x. (KnownNat n, Eq x, Conjugate x) => M n n x -> Bool
hermitian (M a) = Data.all
  do \(i, j) -> a ! i ! j == conjugate (a ! j ! i)
  do join (liftM2 (,)) (dimensions @n).getConst
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
