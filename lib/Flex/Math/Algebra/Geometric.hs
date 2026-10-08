{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Flex.Math.Algebra.Geometric where

import Flex.Math.Algebra
import Flex.Math.Basis
import Flex.Math.Category
import Flex.Math.Foldable (length)
import Flex.Math.Matrix
import Flex.Math.Module
import Flex.Math.Numbers

import Data.Bool
import Data.Eq
import Data.Finite (finites)
import Data.Function (on)
import Data.Kind
import Data.List qualified as List
import Data.List1 qualified as List1
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord
import GHC.Generics (Generic)
import Text.Show (Show)

class
  ( From (v x) (Multi v x)
  , Basis v x
  , Sesquilinear (v x)
  , Algebra (Multi v x)
  ) =>
  Geometric v x
  where
  data Multi v x :: Type

instance (KnownNat n, Eq x, Ring x, Conjugate x) => Geometric (V n) x where
  data Multi (V n) x = Multi {unMulti :: Map [Finite n] x}
    deriving (Eq, Ord, Show)

(!) ::
  forall n x.
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multi (V n) x -> [Finite n] -> x
Multi u ! fs = signature fs * Map.findWithDefault zero (List.sort fs) u
 where
  signature :: [Finite n] -> x
  signature (a : b : f) = case compare a b of
    GT -> negative one * signature (b : f)
    _ -> signature (b : f)
  signature _ = one

unit :: (KnownNat n, Eq x, Ring x, Conjugate x) => Multi (V n) x
unit = Multi (Map.fromList [([], one)])

canonical ::
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  [([Finite n], x)] -> Multi (V n) x
canonical = (one *) . Multi . Map.fromList

pseudoscalar ::
  forall n x.
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multi (V n) x
pseudoscalar = canonical [(finites, one)]

inversePseudoscalar ::
  forall n x.
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multi (V n) x
inversePseudoscalar =
  canonical [(finites, if even (natVal (Proxy @n)) then negative one else one)]

kVector :: forall n x. (KnownNat n) => Natural -> Multi (V n) x -> Multi (V n) x
kVector k (Multi u) = Multi (ifilter @[Finite n] (\fs _ -> length fs == k) u)

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  From (V n x) (Multi (V n) x)
  where
  from :: V n x -> Multi (V n) x
  from = ifoldl @Integer (\i multi x -> multi + canonical [([from i], x)]) zero

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Addition (Multi (V n) x) (Multi (V n) x) (Multi (V n) x)
  where
  (+.) :: Multi (V n) x -> Multi (V n) x -> Multi (V n) x
  Multi u +. Multi v = (Multi . filter (/= zero)) do
    on
      (Map.mergeWithKey (\_ a b -> Just (a + b)) id id)
      unMulti
      (canonical (Map.assocs u))
      (canonical (Map.assocs v))
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Additive (Multi (V n) x)
  where
  zero :: (KnownNat n, Eq x, Ring x, Conjugate x) => Multi (V n) x
  zero = Multi Map.empty
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  AdditiveAbelian (Multi (V n) x)
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Subtraction (Multi (V n) x) (Multi (V n) x) (Multi (V n) x)
  where
  (-.) :: Multi (V n) x -> Multi (V n) x -> Multi (V n) x
  u -. v = u + negative v
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  AdditiveGroup (Multi (V n) x)
  where
  negative :: Multi (V n) x -> Multi (V n) x
  negative (Multi v) = Multi (morphism negative v)

basisMul ::
  (Ord f, AdditiveGroup x, Multiplication x x x) =>
  ([f], x) -> ([f], x) -> ([f], x)
basisMul (s, m) (t, n) = gnome [] (s <> t) (m * n)
 where
  gnome pre (a : b : rest) c = case compare a b of
    LT -> gnome (pre <> [a]) (b : rest) c
    EQ -> back pre rest c
    GT -> back pre (b : a : rest) (negative c)
   where
    back p r c' = case List1.list1 p of
      Nothing -> gnome [] r c'
      Just p' -> gnome (List1.init p') (List1.last p' : r) c'
  gnome pre rest c = (pre <> rest, c)

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplication (Multi (V n) x) (Multi (V n) x) (Multi (V n) x)
  where
  (*.) :: Multi (V n) x -> Multi (V n) x -> Multi (V n) x
  Multi v *. Multi w = (Multi . filter (/= zero)) do
    Map.fromListWith (+) (liftA2 basisMul (Map.assocs v) (Map.assocs w))
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplicative (Multi (V n) x)
  where
  one :: Multi (V n) x
  one = Multi (Map.singleton [] one)

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Distributive (Multi (V n) x)

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplication x (Multi (V n) x) (Multi (V n) x)
  where
  (*.) :: x -> Multi (V n) x -> Multi (V n) x
  x *. Multi v = Multi (morphism (x *) v)
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplication (Multi (V n) x) x (Multi (V n) x)
  where
  (*.) :: Multi (V n) x -> x -> Multi (V n) x
  Multi v *. x = Multi (morphism (* x) v)
instance
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  Division (Multi (V n) x) x (Multi (V n) x)
  where
  (/.) :: Multi (V n) x -> x -> Multi (V n) x
  Multi v /. x = Multi (morphism (/ x) v)

instance (From y x) => From y (Scalar (Multi (V n) x)) where
  from :: y -> Scalar (Multi (V n) x)
  from n = ScalarMulti (from n)
  {-# INLINE from #-}

instance
  (Addition x x x) =>
  Addition
    (Scalar (Multi (V n) x))
    (Scalar (Multi (V n) x))
    (Scalar (Multi (V n) x))
  where
  (+.) ::
    Scalar (Multi (V n) x) -> Scalar (Multi (V n) x) -> Scalar (Multi (V n) x)
  ScalarMulti x +. ScalarMulti y = ScalarMulti (x + y)
  {-# INLINE (+.) #-}
instance
  (Subtraction x x x) =>
  Subtraction
    (Scalar (Multi (V n) x))
    (Scalar (Multi (V n) x))
    (Scalar (Multi (V n) x))
  where
  (-.) ::
    Scalar (Multi (V n) x) -> Scalar (Multi (V n) x) -> Scalar (Multi (V n) x)
  ScalarMulti x -. ScalarMulti y = ScalarMulti (x - y)
  {-# INLINE (-.) #-}
instance
  (Multiplication x x x) =>
  Multiplication
    (Scalar (Multi (V n) x))
    (Scalar (Multi (V n) x))
    (Scalar (Multi (V n) x))
  where
  (*.) ::
    Scalar (Multi (V n) x) -> Scalar (Multi (V n) x) -> Scalar (Multi (V n) x)
  ScalarMulti x *. ScalarMulti y = ScalarMulti (x * y)
  {-# INLINE (*.) #-}
instance
  (Division x x x) =>
  Division
    (Scalar (Multi (V n) x))
    (Scalar (Multi (V n) x))
    (Scalar (Multi (V n) x))
  where
  (/.) ::
    Scalar (Multi (V n) x) -> Scalar (Multi (V n) x) -> Scalar (Multi (V n) x)
  ScalarMulti x /. ScalarMulti y = ScalarMulti (x / y)
  {-# INLINE (/.) #-}
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplication (Scalar (Multi (V n) x)) (Multi (V n) x) (Multi (V n) x)
  where
  (*.) :: Scalar (Multi (V n) x) -> Multi (V n) x -> Multi (V n) x
  ScalarMulti k *. x = k *. x
  {-# INLINE (*.) #-}
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplication (Multi (V n) x) (Scalar (Multi (V n) x)) (Multi (V n) x)
  where
  (*.) :: Multi (V n) x -> Scalar (Multi (V n) x) -> Multi (V n) x
  x *. ScalarMulti k = x *. k
  {-# INLINE (*.) #-}
instance
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  Division (Multi (V n) x) (Scalar (Multi (V n) x)) (Multi (V n) x)
  where
  (/.) :: Multi (V n) x -> Scalar (Multi (V n) x) -> Multi (V n) x
  x /. ScalarMulti k = x /. k
  {-# INLINE (/.) #-}
instance (Power x r x) => Power (Scalar (Multi (V n) x)) r (Scalar (Multi (V n) x)) where
  (^) :: Scalar (Multi (V n) x) -> r -> Scalar (Multi (V n) x)
  ScalarMulti x ^ r = ScalarMulti (x ^ r)
  {-# INLINE (^) #-}
instance (Absolute x x) => Absolute (Scalar (Multi (V n) x)) x where
  absolute :: Scalar (Multi (V n) x) -> x
  absolute (ScalarMulti k) = absolute k
  {-# INLINE absolute #-}
instance (Absolute x x) => Absolute (Scalar (Multi (V n) x)) (Scalar (Multi (V n) x)) where
  absolute :: Scalar (Multi (V n) x) -> Scalar (Multi (V n) x)
  absolute (ScalarMulti k) = ScalarMulti (absolute k)
  {-# INLINE absolute #-}

instance
  (Additive x, Subtraction x x x, Multiplicative x) =>
  Semiring (Scalar (Multi (V n) x))
instance
  (AdditiveAbelian x, AdditiveGroup x, Multiplicative x) =>
  Ring (Scalar (Multi (V n) x))
instance (Domain x) => Domain (Scalar (Multi (V n) x))

instance (KnownNat n, Eq x, Ring x, Conjugate x) => Module (Multi (V n) x) where
  newtype Scalar (Multi (V n) x) = ScalarMulti {unScalar :: x}
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
instance (KnownNat n, Eq x, Field x, Conjugate x) => Vector (Multi (V n) x)
instance (KnownNat n, Eq x, Ring x, Conjugate x) => Algebra (Multi (V n) x)

wedge ::
  (KnownNat n, Eq x, AdditiveGroup x, Multiplication x x x) =>
  Multi (V n) x -> Multi (V n) x -> Multi (V n) x
wedge (Multi u) (Multi v) = (Multi . filter (/= zero)) do
  Map.fromListWith (+) (justs id (liftA2 f (Map.assocs u) (Map.assocs v)))
 where
  f a@(s, _) b@(t, _)
    | length s + length t == length w = Just c
    | otherwise = Nothing
   where
    c@(w, _) = basisMul a b

dot ::
  (KnownNat n, Eq x, AdditiveGroup x, Multiplication x x x) =>
  Multi (V n) x -> Multi (V n) x -> Multi (V n) x
dot (Multi u) (Multi v) = (Multi . filter (/= zero)) do
  Map.fromListWith (+) (justs id (liftA2 f (Map.assocs u) (Map.assocs v)))
 where
  f a@(s, _) b@(t, _)
    | length t == length s + length w = Just c
    | otherwise = Nothing
   where
    c@(w, _) = basisMul a b
