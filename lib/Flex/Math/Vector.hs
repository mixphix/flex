{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Flex.Math.Vector
  ( V (V, unV)
  , data V1
  , data V2
  , data V3
  , data V4
  , upto
  , (++)
  , vn
  , vnA
  , (!)
  , setV
  , cons
  , uncons
  , snoc
  , unsnoc
  , shifted
  , reverse
  , toList
  , fromList
  , projection
  , householder
  , orthogonalize
  , orthonormalize
  , Scalar (..)
  ) where

import Flex.Math.Category
import Flex.Math.Foldable
import Flex.Math.Module
import Flex.Math.Numbers
import Flex.Math.Optics

import Control.Applicative qualified as Control
import Data.Bool
import Data.Enum
import Data.Eq
import Data.Finite (Finite, finite, getFinite)
import Data.Foldable qualified as Data
import Data.Function (fix, flip)
import Data.Functor qualified as Data
import Data.Functor.Const (Const (Const))
import Data.Kind (Type)
import Data.List qualified as List
import Data.Maybe
import Data.Ord (Ord (..))
import Data.Proxy
import Data.Semigroup (Endo (..))
import Data.Traversable qualified as Data
import Data.Type.Equality (type (~))
import Data.Type.Ord
import Data.Vector.Strict qualified as Vector.Strict
import GHC.Err (error)
import GHC.Generics (Generic)
import GHC.TypeNats
import Text.Show

type V :: Nat -> Type -> Type
newtype V n x = V {unV :: Vector.Strict.Vector x}
  deriving (Eq, Ord, Data.Functor, Data.Foldable, Data.Traversable)

instance (KnownNat n, Show x) => Show (V n x) where
  showsPrec :: Int -> V n x -> ShowS
  showsPrec prec v = showParen (prec > 10) do
    showString "V { "
      . appEndo (foldWith (\x -> Endo do showsPrec 11 x . showString " ") v)
      . showString "}"
  {-# INLINE showsPrec #-}

pattern V1 :: x -> V 1 x
pattern V1 x = V [x]
{-# COMPLETE V1 #-}

pattern V2 :: x -> x -> V 2 x
pattern V2 x y = V [x, y]
{-# COMPLETE V2 #-}

pattern V3 :: x -> x -> x -> V 3 x
pattern V3 x y z = V [x, y, z]
{-# COMPLETE V3 #-}

pattern V4 :: x -> x -> x -> x -> V 4 x
pattern V4 x y z w = V [x, y, z, w]
{-# COMPLETE V4 #-}

(++) :: V m x -> V n x -> V (m + n) x
V u ++ V v = V (u <> v)
{-# INLINE (++) #-}

vn :: forall n x. (KnownNat n) => (Natural -> x) -> V n x
vn i_x = V do Vector.Strict.generate (from (natVal (Proxy @n))) (i_x . from)
{-# INLINE vn #-}

vnA :: forall g n x. (KnownNat n, Apply g) => (Natural -> g x) -> g (V n x)
vnA i_gx = morphism V do
  flip fix 0 \rec n ->
    if succ n == natVal (Proxy @n)
      then morphism Vector.Strict.singleton (i_gx n)
      else liftA2 (Vector.Strict.cons) (i_gx n) (rec (succ n))
{-# INLINE vnA #-}

upto :: forall n. (KnownNat n) => Const [Natural] n
upto = Const [0 .. natVal (Proxy @n) - 1]
{-# INLINE upto #-}

(!) :: V n x -> Natural -> x
(V v) ! n = v Vector.Strict.! from n
{-# INLINE (!) #-}

setV :: Natural -> x -> V n x -> V n x
setV n x (V v) = V do v Vector.Strict.// [(from n, x)]
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

cons :: (KnownNat n) => x -> V n x -> V (1 + n) x
cons x (V v) = V (Vector.Strict.cons x v)
{-# INLINE cons #-}

uncons :: forall n x. (KnownNat n) => V (1 + n) x -> (x, V n x)
uncons (V v) = case Vector.Strict.uncons v of
  Nothing -> error "Flex.Math.Vector.uncons: empty V"
  Just (x, v') -> (x, V v')
{-# INLINE uncons #-}

snoc :: (KnownNat n) => V n x -> x -> V (n + 1) x
snoc (V v) x = V (Vector.Strict.snoc v x)
{-# INLINE snoc #-}

unsnoc :: forall n x. (KnownNat n) => V (n + 1) x -> (V n x, x)
unsnoc (V v) = case Vector.Strict.unsnoc v of
  Nothing -> error "Flex.Math.Vector.uncons: empty V"
  Just (v', x) -> (V v', x)
{-# INLINE unsnoc #-}

shifted ::
  forall n x.
  ( KnownNat n
  , (1 + n) ~ (n + 1)
  , Eq x
  ) =>
  V (1 + n) x -> V (n + 1) x -> Bool
shifted = shifted' (natVal (Proxy @n))
 where
  shifted' k u v
    | k == 0 = u == v
    | otherwise =
        u == v || case uncons u of
          (u0, u') -> shifted' (pred k) (snoc u' u0) v
{-# INLINE shifted #-}

reverse :: forall n x. (KnownNat n) => V n x -> V n x
reverse v = vn @n \i -> v ! (natVal (Proxy @n) - 1 - i)
{-# INLINE reverse #-}

instance Morphisms (->) (->) (V n) where
  morphism :: (x -> y) -> V n x -> V n y
  morphism = Data.fmap
  {-# INLINE morphism #-}
instance Folds (->) (->) (V n) where
  foldWith :: (Monoid z) => (x -> z) -> V n x -> z
  foldWith = Data.foldMap'
  {-# INLINE foldWith #-}
instance (KnownNat n, 1 <= n, 1 + (n - 1) ~ n) => Folds1 (->) (->) (V n) where
  foldWith1 :: (Semigroup z) => (x -> z) -> V n x -> z
  foldWith1 x_z v = case uncons @(n - 1) v of
    (x, v') -> case foldWith (Just . x_z) v' of
      Nothing -> x_z x
      Just z -> x_z x <> z
instance Traversals (->) (->) (V n) where
  traverse :: (Applicative g) => (x -> g y) -> V n x -> g (V n y)
  traverse x_gy (V v) = morphism V (traverse x_gy v)
  {-# INLINE traverse #-}
instance (KnownNat n, 1 <= n, 1 + (n - 1) ~ n) => Traversals1 (->) (->) (V n) where
  traverse1 :: (Apply g) => (x -> g y) -> V n x -> g (V n y)
  traverse1 x_gy (V v) = morphism V case traverse1F x_gy v of
    NothingF _ -> error "Flex.Math.Vector.traverse1: empty V"
    JustF gx -> gx
  {-# INLINE traverse1 #-}

instance Morphisms (Ix Natural) (->) (V n) where
  morphism :: Ix Natural x y -> V n x -> V n y
  morphism (Ix i_x_y) (V v) = V (morphism (Ix i_x_y) v)
  {-# INLINE morphism #-}
instance Morphisms (Ix Integer) (->) (V n) where
  morphism :: Ix Integer x y -> V n x -> V n y
  morphism (Ix i_x_y) (V v) = V (morphism (Ix i_x_y) v)
  {-# INLINE morphism #-}
instance Morphisms (Ix Int) (->) (V n) where
  morphism :: Ix Int x y -> V n x -> V n y
  morphism (Ix i_x_y) (V v) = V (morphism (Ix i_x_y) v)
  {-# INLINE morphism #-}
instance Folds (Ix Natural) (->) (V n) where
  foldWith :: (Monoid z) => Ix Natural x z -> V n x -> z
  foldWith (Ix i_x_z) (V v) = foldWith (Ix i_x_z) v
  {-# INLINE foldWith #-}
instance Folds (Ix Integer) (->) (V n) where
  foldWith :: (Monoid z) => Ix Integer x z -> V n x -> z
  foldWith (Ix i_x_z) (V v) = foldWith (Ix i_x_z) v
  {-# INLINE foldWith #-}
instance Folds (Ix Int) (->) (V n) where
  foldWith :: (Monoid z) => Ix Int x z -> V n x -> z
  foldWith (Ix i_x_z) (V v) = foldWith (Ix i_x_z) v
  {-# INLINE foldWith #-}
instance (KnownNat n, 1 <= n, 1 + (n - 1) ~ n) => Folds1 (Ix Natural) (->) (V n) where
  foldWith1 :: (Semigroup z) => Ix Natural x z -> V n x -> z
  foldWith1 (Ix i_x_z) v = case uncons @(n - 1) v of
    (x, v') -> case foldWith (Ix do (Just .) . i_x_z . succ) v' of
      Nothing -> i_x_z 0 x
      Just z -> i_x_z 0 x <> z
  {-# INLINE foldWith1 #-}
instance (KnownNat n, 1 <= n, 1 + (n - 1) ~ n) => Folds1 (Ix Integer) (->) (V n) where
  foldWith1 :: (Semigroup z) => Ix Integer x z -> V n x -> z
  foldWith1 (Ix i_x_z) v = case uncons @(n - 1) v of
    (x, v') -> case foldWith (Ix do (Just .) . i_x_z . succ) v' of
      Nothing -> i_x_z 0 x
      Just z -> i_x_z 0 x <> z
  {-# INLINE foldWith1 #-}
instance (KnownNat n, 1 <= n, 1 + (n - 1) ~ n) => Folds1 (Ix Int) (->) (V n) where
  foldWith1 :: (Semigroup z) => Ix Int x z -> V n x -> z
  foldWith1 (Ix i_x_z) v = case uncons @(n - 1) v of
    (x, v') -> case foldWith (Ix do (Just .) . i_x_z . succ) v' of
      Nothing -> i_x_z 0 x
      Just z -> i_x_z 0 x <> z
  {-# INLINE foldWith1 #-}
instance Traversals (Ix Natural) (->) (V n) where
  traverse ::
    (Applicative g) => Ix Natural x (g y) -> V n x -> g (V n y)
  traverse (Ix i_x_gy) (V v) = morphism V (traverse (Ix i_x_gy) v)
  {-# INLINE traverse #-}
instance Traversals (Ix Integer) (->) (V n) where
  traverse ::
    (Applicative g) => Ix Integer x (g y) -> V n x -> g (V n y)
  traverse (Ix i_x_gy) (V v) = morphism V (traverse (Ix i_x_gy) v)
  {-# INLINE traverse #-}
instance Traversals (Ix Int) (->) (V n) where
  traverse ::
    (Applicative g) => Ix Int x (g y) -> V n x -> g (V n y)
  traverse (Ix i_x_gy) (V v) = morphism V (traverse (Ix i_x_gy) v)
  {-# INLINE traverse #-}
instance (KnownNat n, 1 <= n, 1 + (n - 1) ~ n) => Traversals1 (Ix Natural) (->) (V n) where
  traverse1 ::
    (Apply g) => Ix Natural x (g y) -> V n x -> g (V n y)
  traverse1 (Ix i_x_gy) (V v) = morphism V case itraverse1F i_x_gy v of
    NothingF _ -> error "Flex.Math.Vector.itraverse1: empty V"
    JustF gx -> gx
  {-# INLINE traverse1 #-}
instance (KnownNat n, 1 <= n, 1 + (n - 1) ~ n) => Traversals1 (Ix Integer) (->) (V n) where
  traverse1 ::
    (Apply g) => Ix Integer x (g y) -> V n x -> g (V n y)
  traverse1 (Ix i_x_gy) (V v) = morphism V case itraverse1F i_x_gy v of
    NothingF _ -> error "Flex.Math.Vector.itraverse1: empty V"
    JustF gx -> gx
  {-# INLINE traverse1 #-}
instance (KnownNat n, 1 <= n, 1 + (n - 1) ~ n) => Traversals1 (Ix Int) (->) (V n) where
  traverse1 ::
    (Apply g) => Ix Int x (g y) -> V n x -> g (V n y)
  traverse1 (Ix i_x_gy) (V v) = morphism V case itraverse1F i_x_gy v of
    NothingF _ -> error "Flex.Math.Vector.itraverse1: empty V"
    JustF gx -> gx
  {-# INLINE traverse1 #-}
instance (KnownNat n) => Pure (V n) where
  pure :: x -> V n x
  pure x = V (Vector.Strict.replicate (from (natVal (Proxy @n))) x)
  {-# INLINE pure #-}
instance Apply (V n) where
  liftA2 :: (x -> y -> z) -> V n x -> V n y -> V n z
  liftA2 x_y_z (V u) (V v) = V (Vector.Strict.zipWith x_y_z u v)
  {-# INLINE liftA2 #-}
instance (KnownNat n) => Control.Applicative (V n) where
  pure :: x -> V n x
  pure = pure
  {-# INLINE pure #-}
  (<*>) :: V n (x -> y) -> V n x -> V n y
  (<*>) = liftA2 id
  {-# INLINE (<*>) #-}

toList :: V n x -> [x]
toList (V v) = Vector.Strict.toList v
{-# INLINE toList #-}

fromList :: forall n x. (KnownNat n) => [x] -> Maybe (V n x)
fromList xs
  | length xs == natVal (Proxy @n) =
      pure (V (Vector.Strict.fromListN (from (natVal (Proxy @n))) xs))
  | otherwise = nil
{-# INLINE fromList #-}

instance Indices (V n x) where
  type Index (V n x) = Natural
  type Value (V n x) = x
  index :: Natural -> Traversal' (V n x) x
  index i = lens (! i) (flip (setV i))
  {-# INLINE index #-}
instance Each (V n x) (V n y) x y
instance IxEach Natural (V n x) (V n y) x y

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
  gramSchmidt1 ns [] = List.reverse ns
  gramSchmidt1 ns (v : vs) =
    let u = v + sumOn (negative . (`projection` v)) ns
     in gramSchmidt1 (u : ns) vs
{-# INLINE orthogonalize #-}

orthonormalize ::
  (KnownNat n, Field x, Conjugate x, Root x) =>
  [V n x] -> [V n x]
orthonormalize = morphism normalize . orthogonalize
{-# INLINE orthonormalize #-}
