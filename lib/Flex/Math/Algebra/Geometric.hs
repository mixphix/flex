{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Flex.Math.Algebra.Geometric
  ( Multi (Multi, unMulti)
  , (!)
  , scalar
  , canonical
  , pseudoscalar
  , grade
  , ungrade0
  , Scalar (..)
  , wedge
  , dot
  , dagger
  , hat
  , flipGrade
  ) where

import Flex.Math.Algebra
import Flex.Math.Basis
import Flex.Math.Category
import Flex.Math.Foldable (length, product)
import Flex.Math.Matrix hiding ((!))
import Flex.Math.Module
import Flex.Math.Numbers

import Control.Exception
import Data.Bool
import Data.Eq
import Data.Finite (finites)
import Data.List qualified as List
import Data.List1 qualified as List1
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord
import GHC.Generics (Generic)
import GHC.TypeNats
import Text.Show (Show)

data Multi n x = Multi {unMulti :: Map [Finite n] x}
  deriving (Eq, Ord, Show)

instance Morphisms (->) (->) (Multi n) where
  morphism :: (x -> y) -> Multi n x -> Multi n y
  morphism x_y (Multi u) = Multi (morphism x_y u)
instance Folds (->) (->) (Multi n) where
  foldWith :: (Monoid z) => (x -> z) -> Multi n x -> z
  foldWith x_z (Multi u) = foldWith x_z u
instance Traversals (->) (->) (Multi n) where
  traverse :: (Applicative g) => (x -> g y) -> Multi n x -> g (Multi n y)
  traverse x_gy (Multi u) = morphism Multi (traverse x_gy u)
instance Morphisms (Ix [Finite n]) (->) (Multi n) where
  morphism :: Ix [Finite n] x y -> Multi n x -> Multi n y
  morphism (Ix fs_x_y) (Multi u) = Multi (morphism (Ix fs_x_y) u)
instance Folds (Ix [Finite n]) (->) (Multi n) where
  foldWith :: (Monoid z) => Ix [Finite n] x z -> Multi n x -> z
  foldWith (Ix fs_x_y) (Multi u) = foldWith (Ix fs_x_y) u
instance Traversals (Ix [Finite n]) (->) (Multi n) where
  traverse ::
    (Applicative g) => Ix [Finite n] x (g y) -> Multi n x -> g (Multi n y)
  traverse (Ix fs_x_gy) (Multi u) = morphism Multi (traverse (Ix fs_x_gy) u)

(!) ::
  forall n x.
  (KnownNat n, Ring x, Conjugate x) =>
  Multi n x -> [Finite n] -> x
Multi u ! fs = signature fs * Map.findWithDefault zero (List.sort fs) u
 where
  signature :: [Finite n] -> x
  signature (a : b : f) = case compare a b of
    GT -> negative one * signature (b : f)
    _ -> signature (b : f)
  signature _ = one

scalar :: (KnownNat n, Eq x, Ring x, Conjugate x) => x -> Multi n x
scalar x = Multi (Map.fromList [([], x)])

canonical ::
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  [([Finite n], x)] -> Multi n x
canonical = (one *) . Multi . Map.fromList

pseudoscalar ::
  forall n x.
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  x -> Multi n x
pseudoscalar x = canonical [(finites, x)]

grade :: forall n x. (KnownNat n) => Natural -> Multi n x -> Multi n x
grade k (Multi u) = Multi (ifilter @[Finite n] (\fs _ -> length fs == k) u)

ungrade0 :: Multi n x -> Maybe x
ungrade0 (Multi u) =
  Map.lookupMin u >>= \case
    ([], k) | Map.null (Map.delete [] u) -> pure k
    _ -> nil

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  From (V n x) (Multi n x)
  where
  from :: V n x -> Multi n x
  from = ifoldl @Integer (\i multi x -> multi + canonical [([from i], x)]) zero

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Addition (Multi n x) (Multi n x) (Multi n x)
  where
  (+.) :: Multi n x -> Multi n x -> Multi n x
  Multi u +. Multi v = (Multi . filter (/= zero)) do
    Map.mergeWithKey (\_ a b -> Just (a + b)) id id u v
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Additive (Multi n x)
  where
  zero :: (KnownNat n, Eq x, Ring x, Conjugate x) => Multi n x
  zero = Multi Map.empty
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  AdditiveAbelian (Multi n x)
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Subtraction (Multi n x) (Multi n x) (Multi n x)
  where
  (-.) :: Multi n x -> Multi n x -> Multi n x
  u -. v = u + negative v
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  AdditiveGroup (Multi n x)
  where
  negative :: Multi n x -> Multi n x
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
  Multiplication (Multi n x) (Multi n x) (Multi n x)
  where
  (*.) :: Multi n x -> Multi n x -> Multi n x
  Multi v *. Multi w = (Multi . filter (/= zero)) do
    Map.fromListWith (+) (liftA2 basisMul (Map.assocs v) (Map.assocs w))
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplicative (Multi n x)
  where
  one :: Multi n x
  one = Multi (Map.singleton [] one)

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Distributive (Multi n x)

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplication x (Multi n x) (Multi n x)
  where
  (*.) :: x -> Multi n x -> Multi n x
  x *. Multi v = Multi (morphism (x *) v)
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplication (Multi n x) x (Multi n x)
  where
  (*.) :: Multi n x -> x -> Multi n x
  Multi v *. x = Multi (morphism (* x) v)
instance
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  Division (Multi n x) x (Multi n x)
  where
  (/.) :: Multi n x -> x -> Multi n x
  Multi v /. x = Multi (morphism (/ x) v)

instance (From y x) => From y (Scalar (Multi n x)) where
  from :: y -> Scalar (Multi n x)
  from n = ScalarMulti (from n)
  {-# INLINE from #-}

instance
  (Addition x x x) =>
  Addition
    (Scalar (Multi n x))
    (Scalar (Multi n x))
    (Scalar (Multi n x))
  where
  (+.) ::
    Scalar (Multi n x) -> Scalar (Multi n x) -> Scalar (Multi n x)
  ScalarMulti x +. ScalarMulti y = ScalarMulti (x + y)
  {-# INLINE (+.) #-}
instance
  (Subtraction x x x) =>
  Subtraction
    (Scalar (Multi n x))
    (Scalar (Multi n x))
    (Scalar (Multi n x))
  where
  (-.) ::
    Scalar (Multi n x) -> Scalar (Multi n x) -> Scalar (Multi n x)
  ScalarMulti x -. ScalarMulti y = ScalarMulti (x - y)
  {-# INLINE (-.) #-}
instance
  (Multiplication x x x) =>
  Multiplication
    (Scalar (Multi n x))
    (Scalar (Multi n x))
    (Scalar (Multi n x))
  where
  (*.) ::
    Scalar (Multi n x) -> Scalar (Multi n x) -> Scalar (Multi n x)
  ScalarMulti x *. ScalarMulti y = ScalarMulti (x * y)
  {-# INLINE (*.) #-}
instance
  (Division x x x) =>
  Division
    (Scalar (Multi n x))
    (Scalar (Multi n x))
    (Scalar (Multi n x))
  where
  (/.) ::
    Scalar (Multi n x) -> Scalar (Multi n x) -> Scalar (Multi n x)
  ScalarMulti x /. ScalarMulti y = ScalarMulti (x / y)
  {-# INLINE (/.) #-}
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplication (Scalar (Multi n x)) (Multi n x) (Multi n x)
  where
  (*.) :: Scalar (Multi n x) -> Multi n x -> Multi n x
  ScalarMulti k *. x = k *. x
  {-# INLINE (*.) #-}
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplication (Multi n x) (Scalar (Multi n x)) (Multi n x)
  where
  (*.) :: Multi n x -> Scalar (Multi n x) -> Multi n x
  x *. ScalarMulti k = x *. k
  {-# INLINE (*.) #-}
instance
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  Division (Multi n x) (Scalar (Multi n x)) (Multi n x)
  where
  (/.) :: Multi n x -> Scalar (Multi n x) -> Multi n x
  x /. ScalarMulti k = x /. k
  {-# INLINE (/.) #-}
instance (Power x r x) => Power (Scalar (Multi n x)) r (Scalar (Multi n x)) where
  (^) :: Scalar (Multi n x) -> r -> Scalar (Multi n x)
  ScalarMulti x ^ r = ScalarMulti (x ^ r)
  {-# INLINE (^) #-}
instance (Absolute x x) => Absolute (Scalar (Multi n x)) x where
  absolute :: Scalar (Multi n x) -> x
  absolute (ScalarMulti k) = absolute k
  {-# INLINE absolute #-}
instance (Absolute x x) => Absolute (Scalar (Multi n x)) (Scalar (Multi n x)) where
  absolute :: Scalar (Multi n x) -> Scalar (Multi n x)
  absolute (ScalarMulti k) = ScalarMulti (absolute k)
  {-# INLINE absolute #-}

instance
  (Additive x, Subtraction x x x, Multiplicative x) =>
  Semiring (Scalar (Multi n x))
instance
  (AdditiveAbelian x, AdditiveGroup x, Multiplicative x) =>
  Ring (Scalar (Multi n x))
instance (Domain x) => Domain (Scalar (Multi n x))

instance (KnownNat n, Eq x, Ring x, Conjugate x) => Module (Multi n x) where
  newtype Scalar (Multi n x) = ScalarMulti {unScalar :: x}
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
instance (KnownNat n, Eq x, Field x, Conjugate x) => Vector (Multi n x)
instance (KnownNat n, Eq x, Ring x, Conjugate x) => Algebra (Multi n x)

instance (KnownNat n) => Collectable (Multi n) where
  type Collectability (Multi n) = C2 Ring Conjugate
  distribute ::
    (Along f, Collectability (Multi n) x) => f (Multi n x) -> Multi n (f x)
  distribute fm = Multi do
    Map.fromList [(fs, morphism (! fs) fm) | fs <- List.subsequences finites]
instance (KnownNat n) => Tabulation (Multi n) where
  type Table (Multi n) = Finite (2 ^ n)
  fromTable :: (Collectability (Multi n) x) => (Finite (2 ^ n) -> x) -> Multi n x
  fromTable fi = Multi do
    Map.fromList
      [ (f, fi (from i))
      | (i, f) <- List.zip [0 :: Integer ..] (List.subsequences finites)
      ]
  toTable :: (Collectability (Multi n) x) => Multi n x -> Finite (2 ^ n) -> x
  toTable u f = u ! (List.subsequences (finites @n) List.!! from f)

instance (KnownNat n, Eq x, Ring x, Conjugate x) => Basis (Multi n) x where
  basis :: Finite (2 ^ n) -> Multi n x
  basis f = canonical [(List.subsequences (finites @n) List.!! from f, one)]

wedge ::
  (KnownNat n, Eq x, AdditiveGroup x, Multiplication x x x) =>
  Multi n x -> Multi n x -> Multi n x
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
  Multi n x -> Multi n x -> Multi n x
dot (Multi u) (Multi v) = (Multi . filter (/= zero)) do
  Map.fromListWith (+) (justs id (liftA2 f (Map.assocs u) (Map.assocs v)))
 where
  f a@(s, _) b@(t, _)
    | length t == length s + length w = Just c
    | otherwise = Nothing
   where
    c@(w, _) = basisMul a b

dagger :: (KnownNat n, Eq x, Ring x, Conjugate x) => Multi n x -> Multi n x
dagger (Multi u) = canonical (morphism (morphism' List.reverse) (Map.assocs u))

hat ::
  forall n x.
  (KnownNat n, Eq x, Ring x, Conjugate x, MultiplicativeAbelian x) =>
  Multi n x -> Multi n x
hat (Multi u) = canonical do
  morphism
    ( \(fs, x) ->
        (fs, product (List.replicate (from (length fs)) (negative @x one)) * x)
    )
    (Map.assocs u)

flipGrade ::
  (KnownNat n, Eq x, Ring x, Conjugate x) => Natural -> Multi n x -> Multi n x
flipGrade k m = m - scalar (one + one) * grade k m

instance
  (KnownNat n, Eq x, Ring x, Conjugate x, MultiplicativeAbelian x) =>
  Conjugate (Multi n x)
  where
  conjugate :: Multi n x -> Multi n x
  conjugate = dagger . hat

instance
  (Eq x, Ring x, Conjugate x, Division x x x, MultiplicativeAbelian x) =>
  Division (Multi 2 x) (Multi 2 x) (Multi 2 x)
  where
  (/.) :: Multi 2 x -> Multi 2 x -> Multi 2 x
  u /. v = u * reciprocal v
instance
  (Eq x, Ring x, Conjugate x, Division x x x, MultiplicativeAbelian x) =>
  MultiplicativeGroup (Multi 2 x)
  where
  reciprocal :: Multi 2 x -> Multi 2 x
  reciprocal v =
    conjugate v /. case ungrade0 (v * conjugate v) of
      Nothing -> throw DivideByZero
      Just sc -> sc

instance
  (Eq x, Ring x, Conjugate x, Division x x x, MultiplicativeAbelian x) =>
  Division (Multi 3 x) (Multi 3 x) (Multi 3 x)
  where
  (/.) :: Multi 3 x -> Multi 3 x -> Multi 3 x
  u /. v = u * reciprocal v
instance
  (Eq x, Ring x, Conjugate x, Division x x x, MultiplicativeAbelian x) =>
  MultiplicativeGroup (Multi 3 x)
  where
  reciprocal :: Multi 3 x -> Multi 3 x
  reciprocal v =
    let numer = conjugate v * hat v * dagger v
     in numer /. case ungrade0 (v * numer) of
          Nothing -> throw DivideByZero
          Just sc -> sc

instance
  (Eq x, Ring x, Conjugate x, Division x x x, MultiplicativeAbelian x) =>
  Division (Multi 4 x) (Multi 4 x) (Multi 4 x)
  where
  (/.) :: Multi 4 x -> Multi 4 x -> Multi 4 x
  u /. v = u * reciprocal v
instance
  (Eq x, Ring x, Conjugate x, Division x x x, MultiplicativeAbelian x) =>
  MultiplicativeGroup (Multi 4 x)
  where
  reciprocal :: Multi 4 x -> Multi 4 x
  reciprocal v =
    let numer = conjugate v * flipGrade 3 (flipGrade 4 (v * conjugate v))
     in numer /. case ungrade0 (v * numer) of
          Nothing -> throw DivideByZero
          Just sc -> sc

instance
  (Eq x, Ring x, Conjugate x, Division x x x, MultiplicativeAbelian x) =>
  Division (Multi 5 x) (Multi 5 x) (Multi 5 x)
  where
  (/.) :: Multi 5 x -> Multi 5 x -> Multi 5 x
  u /. v = u * reciprocal v
instance
  (Eq x, Ring x, Conjugate x, Division x x x, MultiplicativeAbelian x) =>
  MultiplicativeGroup (Multi 5 x)
  where
  reciprocal :: Multi 5 x -> Multi 5 x
  reciprocal v =
    let numer =
          conjugate v
            * hat v
            * dagger v
            * flipGrade 1 (flipGrade 4 (v * conjugate v * hat v * dagger v))
     in numer /. case ungrade0 (v * numer) of
          Nothing -> throw DivideByZero
          Just sc -> sc
