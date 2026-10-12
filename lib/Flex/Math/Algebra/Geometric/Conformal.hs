{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Flex.Math.Algebra.Geometric.Conformal
  ( inversion
  , half
  , Conformal (Conformal, unConformal)
  , (!)
  , dual
  , wedge
  , dot
  , meet
  , expansion
  , origin
  , originCoeff
  , infinity
  , infinityCoeff
  , ep
  , en
  , minkowskiPlane
  , scalar
  , pseudoscalar
  , canonical
  , grade
  , ungrade0
  , point
  , vectorPart
  , unpoint
  , sphere
  , sphereP
  , line
  , plane
  -- , exp_
  , translate
  , dilate
  , transvert
  , midpoint
  ) where

import Flex.Math.Category
import Flex.Math.Foldable (length)
import Flex.Math.Matrix hiding ((!))
import Flex.Math.Matrix qualified as Matrix
import Flex.Math.Numbers
import Flex.Math.Suspension

import Data.Bool
import Data.Eq
import Data.Finite (finites)
import Data.Function (flip, (&))
import Data.List qualified as List
import Data.List1 qualified as List1
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord
import Data.Semigroup
import Text.Show

half :: (Addition x x x, Multiplicative x, Division x x x) => x -> x
half = (/ (one + one))

newtype Conformal n x = Conformal {unConformal :: Map [Suspension (Finite n)] x}
  deriving (Eq, Ord)

eneg, epos :: (KnownNat n) => Suspension (Finite n)
epos = North
eneg = South

pseud :: (KnownNat n) => [Suspension (Finite n)]
pseud = [South] <> morphism Meridian finites <> [North]

instance
  (KnownNat n, Eq x, Ring x, Conjugate x, Signed x, Show x) =>
  Show (Conformal n x)
  where
  showsPrec :: Int -> Conformal n x -> ShowS
  showsPrec pr u0 =
    let basis = List.subsequences pseud
        u1 =
          [ s . es
          | bv <- basis
          , let u = u0 ! bv
                s = case sign u of
                  Negative -> showString " - " . showsPrec 11 (negative u)
                  Unsigned
                    | u0 == zero && List.null bv -> showString "0"
                    | otherwise -> showString ""
                  Positive -> showString " + " . showsPrec 11 u
                Endo es = flip foldWith bv \f -> Endo do
                  morphism (from @_ @Natural) f & \case
                    South -> case sign u of
                      Negative -> showString "e-"
                      Unsigned -> id
                      Positive -> showString "e-"
                    Meridian n -> case sign u of
                      Negative -> showString "e" . shows (n + 1)
                      Unsigned -> id
                      Positive -> showString "e" . shows (n + 1)
                    North -> case sign u of
                      Negative -> showString "e+"
                      Unsigned -> id
                      Positive -> showString "e+"
          ]
     in showParen (pr > 10) (foldr (.) id u1)

instance Morphisms (->) (->) (Conformal n) where
  morphism :: (x -> y) -> Conformal n x -> Conformal n y
  morphism x_y (Conformal u) = Conformal (morphism x_y u)

(!) ::
  forall n x.
  (KnownNat n, Ring x, Conjugate x) =>
  Conformal n x -> [Suspension (Finite n)] -> x
Conformal u ! fs = signature fs * Map.findWithDefault zero (List.sort fs) u
 where
  signature :: [Suspension (Finite n)] -> x
  signature (a : b : f) = case compare a b of
    GT -> negative one * signature (b : f)
    _ -> signature (b : f)
  signature _ = one

dual ::
  forall n x.
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Conformal n x -> Conformal n x
dual = (* pseudoscalar one)

basisMulConformal ::
  forall n x.
  (KnownNat n, AdditiveGroup x, Multiplication x x x) =>
  ([Suspension (Finite n)], x) ->
  ([Suspension (Finite n)], x) ->
  ([Suspension (Finite n)], x)
basisMulConformal (s, m) (t, n) = gnome [] (s <> t) (m * n)
 where
  gnome pre (a : b : rest) c = case compare a b of
    LT -> gnome (pre <> [a]) (b : rest) c
    EQ -> back pre rest if a == eneg then negative c else c
    GT -> back pre (b : a : rest) (negative c)
   where
    back p = case List1.list1 p of
      Nothing -> gnome []
      Just p' -> gnome (List1.init p') . (List1.last p' :)
  gnome pre rest c = (pre <> rest, c)

wedge ::
  (KnownNat n, Eq x, AdditiveGroup x, Multiplication x x x) =>
  Conformal n x -> Conformal n x -> Conformal n x
wedge (Conformal u) (Conformal v) = (Conformal . filter (/= zero)) do
  Map.fromListWith (+) (justs id (liftA2 f (Map.assocs u) (Map.assocs v)))
 where
  f a@(s, _) b@(t, _) = guard (length s + length t == length w) >> pure c
   where
    c@(w, _) = basisMulConformal a b

dot ::
  (KnownNat n, Eq x, AdditiveGroup x, Multiplication x x x) =>
  Conformal n x -> Conformal n x -> Conformal n x
dot (Conformal u) (Conformal v) = (Conformal . filter (/= zero)) do
  Map.fromListWith (+) (justs id (liftA2 f (Map.assocs u) (Map.assocs v)))
 where
  f a@(s, _) b@(t, _) = guard (length t == length s + length w) >> pure c
   where
    c@(w, _) = basisMulConformal a b

meet ::
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Conformal n x -> Conformal n x -> Conformal n x
meet u v = u `dot` dual v

expansion ::
  forall n x.
  (KnownNat n, Ord x, Ring x, Conjugate x) =>
  Conformal n x -> Conformal n x -> Conformal n x
expansion u v = u `wedge` negative (dual v)

origin ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  x -> Conformal n x
origin x =
  canonical
    [ ([epos], half x)
    , ([eneg], negative (half x))
    ]

originCoeff ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) => Conformal n x -> x
originCoeff u = (maybe zero half . ungrade0) (u `dot` infinity one)

infinity ::
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  x -> Conformal n x
infinity x = canonical [([epos], x), ([eneg], x)]

infinityCoeff ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) => Conformal n x -> x
infinityCoeff u = (maybe zero id . ungrade0) (u `dot` origin one)

minkowskiPlane ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  Conformal n x
minkowskiPlane = origin one `wedge` infinity one

scalar :: (KnownNat n) => x -> Conformal n x
scalar x = Conformal (Map.singleton [] x)

pseudoscalar :: (KnownNat n, Eq x, Ring x, Conjugate x) => x -> Conformal n x
pseudoscalar x = canonical [(List.reverse pseud, x)]

canonical ::
  forall n x.
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  [([Suspension (Finite n)], x)] -> Conformal n x
canonical = (one *) . Conformal . Map.fromList

ep :: (KnownNat n, Multiplicative x) => Conformal n x
ep = Conformal (Map.singleton [epos] one)

en :: (KnownNat n, Multiplicative x) => Conformal n x
en = Conformal (Map.singleton [eneg] one)

grade :: forall n x. (KnownNat n) => Natural -> Conformal n x -> Conformal n x
grade k (Conformal u) = Conformal do
  ifilter @[Suspension (Finite n)] (\fs _ -> length fs == k) u

ungrade0 :: (Additive x) => Conformal n x -> Maybe x
ungrade0 (Conformal u) = case Map.lookupMin u of
  Just ([], k) | Map.null (Map.delete [] u) -> pure k
  Nothing -> pure zero
  _ -> nil

point ::
  forall n x.
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  V n x -> Conformal n x
point u =
  negative (scalar (half one))
    * (vectorPart u - ep)
    * infinity one
    * (vectorPart u - ep)

vectorPart :: (KnownNat n) => V n x -> Conformal n x
vectorPart u = (Conformal . Map.fromList) do
  List.zip (morphism (pure . Meridian) finites) (Matrix.toList u)

unpoint ::
  forall n x.
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  Conformal n x -> Maybe (V n x)
unpoint mu =
  let d = mu ! [eneg] - mu ! [epos]
      obtain s = (mu ! s) / d
   in guard (d /= zero) >> Matrix.fromList do
        morphism (obtain . pure . Meridian) finites

sphere ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  V n x -> x -> Conformal n x
sphere c r = dual do
  point c - scalar ((r * r) / (one + one)) * infinity one

sphereP ::
  forall n x.
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  V n x -> V n x -> V n x -> V n x -> Conformal n x
sphereP p0 p1 p2 p3 =
  point p0 `wedge` point p1 `wedge` point p2 `wedge` point p3

line ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  V n x -> V n x -> Conformal n x
line p q = point p `wedge` point q `wedge` infinity one

plane ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  V n x -> V n x -> V n x -> Conformal n x
plane p q r = point p `wedge` point q `wedge` point r `wedge` infinity one

translate ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  V n x -> Conformal n x -> Conformal n x
translate va x =
  let a = vectorPart (morphism (negative . half) va)
      ta = one + (infinity one * a)
      ta_ = one + (a * infinity one)
   in ta_ * x * ta

dilate ::
  forall n x.
  ( KnownNat n
  , Eq x
  , Ring x
  , Conjugate x
  , Signed x
  , MultiplicativeGroup x
  , Logarithmic x
  , Hyperbolic x
  ) =>
  x -> Conformal n x -> Conformal n x
dilate alpha u0 = case sign alpha of
  Negative -> dilate (negative alpha) case u0 of
    Conformal u -> Conformal do
      imorphism
        @[Suspension (Finite n)]
        ( \basisVectors x ->
            if basisVectors `List.elem` [[epos], [eneg]] then x else negative x
        )
        u
  Unsigned -> origin one
  Positive ->
    let d =
          scalar @n (cosh (half (log alpha)))
            + minkowskiPlane @n @x *. sinh (half (log alpha))
        d_ =
          scalar @n (cosh (half (log alpha)))
            - minkowskiPlane @n @x *. sinh (half (log alpha))
     in d_ * u0 * d

transvert ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  V n x -> Conformal n x -> Conformal n x
transvert va x =
  let a = vectorPart va
      ka = (one - origin (half one) * a)
   in ka * x * ka

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Addition (Conformal n x) (Conformal n x) (Conformal n x)
  where
  (+.) :: Conformal n x -> Conformal n x -> Conformal n x
  Conformal u +. Conformal v = Conformal do
    Map.mergeWithKey (\_ a b -> Just (a + b)) id id u v
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Additive (Conformal n x)
  where
  zero :: Conformal n x
  zero = Conformal Map.empty
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
  negative (Conformal v) = Conformal (morphism negative v)

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplication (Conformal n x) (Conformal n x) (Conformal n x)
  where
  (*.) :: Conformal n x -> Conformal n x -> Conformal n x
  Conformal v *. Conformal w = (Conformal . filter (/= zero)) do
    Map.fromListWith (+) (liftA2 basisMulConformal (Map.assocs v) (Map.assocs w))
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplicative (Conformal n x)
  where
  one :: Conformal n x
  one = scalar one

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Distributive (Conformal n x)

instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplication x (Conformal n x) (Conformal n x)
  where
  (*.) :: x -> Conformal n x -> Conformal n x
  x *. Conformal v = Conformal (morphism (x *) v)
instance
  (KnownNat n, Eq x, Ring x, Conjugate x) =>
  Multiplication (Conformal n x) x (Conformal n x)
  where
  (*.) :: Conformal n x -> x -> Conformal n x
  Conformal v *. x = Conformal (morphism (* x) v)
instance
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  Division (Conformal n x) x (Conformal n x)
  where
  (/.) :: Conformal n x -> x -> Conformal n x
  Conformal v /. x = Conformal (morphism (/ x) v)

inversion ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) => V n x -> V n x
inversion p = fromJust (unpoint (ep * point p * ep))

midpoint ::
  (KnownNat n, Eq x, Ring x, Conjugate x, Division x x x) =>
  V n x -> V n x -> V n x
midpoint u v =
  let x = scalar (half one) * (point u + point v)
      Just xdotn = ungrade0 (join (*) (x `dot` infinity one))
   in (fromJust . unpoint) do
        scalar (negative (half one) / xdotn) * (x * infinity one * x)
