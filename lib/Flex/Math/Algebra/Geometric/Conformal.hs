{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Flex.Math.Algebra.Geometric.Conformal
  ( Conformal (Conformal, unConformal)
  , dual
  , wedge
  , dot
  , origin
  , infinity
  , ep
  , en
  , minkowskiPlane
  , grade
  , ungrade0
  , scalar
  , point
  , conformalToV
  , sphere
  , line
  , plane
  , translate
  ) where

import Flex.Math.Algebra.Geometric hiding (dot, grade, scalar, ungrade0, wedge)
import Flex.Math.Algebra.Geometric qualified as Alg
import Flex.Math.Category
import Flex.Math.Foldable (length)
import Flex.Math.Matrix
import Flex.Math.Matrix qualified as Matrix
import Flex.Math.Module
import Flex.Math.Numbers

import Data.Bool
import Data.Eq
import Data.Finite (finites)
import Data.List qualified as List
import Data.List1 qualified as List1
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord
import GHC.TypeNats
import Text.Show (Show)

newtype Conformal n x = Conformal {unConformal :: Multi (n + 2) x}
  deriving (Eq, Ord, Show)

dual ::
  forall n x.
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Conformal n x -> Conformal n x
dual = (* Conformal (Multi (Map.singleton (finites @(n + 2)) one)))

basisMulConformal ::
  forall n x.
  (KnownNat n, AdditiveGroup x, Multiplication x x x) =>
  ([Finite (n + 2)], x) -> ([Finite (n + 2)], x) -> ([Finite (n + 2)], x)
basisMulConformal (s, m) (t, n) = gnome [] (s <> t) (m * n)
 where
  gnome pre (a : b : rest) c = case compare a b of
    LT -> gnome (pre <> [a]) (b : rest) c
    EQ -> back pre rest if a == from @Integer 1 then negative c else c
    GT -> back pre (b : a : rest) (negative c)
   where
    back p r c' = case List1.list1 p of
      Nothing -> gnome [] r c'
      Just p' -> gnome (List1.init p') (List1.last p' : r) c'
  gnome pre rest c = (pre <> rest, c)

wedge ::
  (KnownNat n, Eq x, AdditiveGroup x, Multiplication x x x) =>
  Conformal n x -> Conformal n x -> Conformal n x
wedge (Conformal (Multi u)) (Conformal (Multi v)) = (Conformal . Multi . filter (/= zero)) do
  Map.fromListWith (+) (justs id (liftA2 f (Map.assocs u) (Map.assocs v)))
 where
  f a@(s, _) b@(t, _)
    | length s + length t == length w = Just c
    | otherwise = Nothing
   where
    c@(w, _) = basisMulConformal a b

dot ::
  (KnownNat n, Eq x, AdditiveGroup x, Multiplication x x x) =>
  Conformal n x -> Conformal n x -> Conformal n x
dot (Conformal (Multi u)) (Conformal (Multi v)) = (Conformal . Multi . filter (/= zero)) do
  Map.fromListWith (+) (justs id (liftA2 f (Map.assocs u) (Map.assocs v)))
 where
  f a@(s, _) b@(t, _)
    | length t == length s + length w = Just c
    | otherwise = Nothing
   where
    c@(w, _) = basisMulConformal a b

origin ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  Conformal n x
origin = Conformal do
  canonical
    [ ([from @Integer 0], negative (one / (one + one)))
    , ([from @Integer 1], one / (one + one))
    ]
infinity ::
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Conformal n x
infinity = Conformal do
  canonical [([from @Integer 0], one), ([from @Integer 1], one)]
minkowskiPlane ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  Conformal n x
minkowskiPlane = wedge origin infinity

scalar :: (KnownNat n) => x -> Conformal n x
scalar x = Conformal (Multi (Map.fromList [([], x)]))

ep :: (KnownNat n, Multiplicative x) => Conformal n x
ep = Conformal (Multi (Map.fromList [([from @Integer 0], one)]))

en :: (KnownNat n, Multiplicative x) => Conformal n x
en = Conformal (Multi (Map.fromList [([from @Integer 1], one)]))

grade :: forall n x. (KnownNat n) => Natural -> Conformal n x -> Conformal n x
grade k (Conformal (Multi u)) = Conformal (Multi (ifilter @[Finite (n + 2)] (\fs _ -> length fs == k) u))

ungrade0 :: Conformal n x -> Maybe x
ungrade0 (Conformal (Multi u)) =
  Map.lookupMin u >>= \case
    ([], k) | Map.null (Map.delete [] u) -> pure k
    _ -> nil

point ::
  forall n x.
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  V n x -> Conformal n x
point u = (Conformal . Multi . Map.fromList) do
  ([from @Integer 0], t - half one)
    : ([from @Integer 1], t + half one)
    : List.zip (morphism (pure . from) [2 .. natVal (Proxy @n) + 1]) (Matrix.toList u)
 where
  half = (/ (one + one))
  t = half (quadrance u).unScalar

conformalToV ::
  forall n x.
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  Conformal n x -> Maybe (V n x)
conformalToV (Conformal mu) =
  let d = mu Alg.! [from @Integer 1] - mu Alg.! [from @Integer 0]
      obtain s = guard (d /= zero) >> pure ((mu Alg.! s) / d)
   in Matrix.fromList @n
        (justs (obtain . pure . from) [2 .. natVal (Proxy @n) + 1])

sphere ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  V n x -> x -> Conformal n x
sphere c r =
  point c - scalar (one / (one + one)) * (scalar r * scalar r) * infinity

line ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  V n x -> V n x -> Conformal n x
line p q = point p `wedge` point q `wedge` infinity

plane ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  V n x -> V n x -> V n x -> Conformal n x
plane p q r = point p `wedge` point q `wedge` point r `wedge` infinity

translate ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  V n x -> Conformal n x
translate t = scalar one + (scalar (one / (one + one)) * point t `wedge` infinity)

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Addition (Conformal n x) (Conformal n x) (Conformal n x)
  where
  (+.) :: Conformal n x -> Conformal n x -> Conformal n x
  Conformal u +. Conformal v = Conformal (u + v)
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Additive (Conformal n x)
  where
  zero :: Conformal n x
  zero = Conformal zero
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  AdditiveAbelian (Conformal n x)
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Subtraction (Conformal n x) (Conformal n x) (Conformal n x)
  where
  (-.) :: Conformal n x -> Conformal n x -> Conformal n x
  u -. v = u + negative v
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  AdditiveGroup (Conformal n x)
  where
  negative :: Conformal n x -> Conformal n x
  negative (Conformal v) = Conformal (negative v)

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplication (Conformal n x) (Conformal n x) (Conformal n x)
  where
  (*.) :: Conformal n x -> Conformal n x -> Conformal n x
  Conformal (Multi v) *. Conformal (Multi w) = (Conformal . Multi . filter (/= zero)) do
    Map.fromListWith (+) (liftA2 basisMulConformal (Map.assocs v) (Map.assocs w))
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplicative (Conformal n x)
  where
  one :: Conformal n x
  one = Conformal one

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Distributive (Conformal n x)

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplication x (Conformal n x) (Conformal n x)
  where
  (*.) :: x -> Conformal n x -> Conformal n x
  x *. Conformal v = Conformal (x *. v)
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplication (Conformal n x) x (Conformal n x)
  where
  (*.) :: Conformal n x -> x -> Conformal n x
  Conformal v *. x = Conformal (v *. x)
instance
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  Division (Conformal n x) x (Conformal n x)
  where
  (/.) :: Conformal n x -> x -> Conformal n x
  Conformal v /. x = Conformal (v /. x)
