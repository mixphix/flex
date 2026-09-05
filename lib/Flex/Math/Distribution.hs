{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Flex.Math.Distribution where

import Flex.Math.Category
import Flex.Math.Foldable
import Flex.Math.Numbers hiding (Fractional)

import Control.Applicative qualified as Control
import Control.Monad qualified as Control
import Data.Bool
import Data.Char (Char)
import Data.Eq
import Data.Function (flip)
import Data.Functor qualified as Data
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as List1 (groupAllWith)
import Data.List1 qualified as List1
import Data.Maybe
import Data.Ord
import Data.String (String)
import Data.Tuple
import GHC.Float (Double)
import GHC.Generics (Generic)
import Text.Printf (printf)
import Text.Show (Show, show)

newtype Probability = Probability {runProbability :: Ration}
  deriving newtype
    ( Eq
    , Ord
    , Show
    , Additive
    , AdditiveAbelian
    , Multiplicative
    , MultiplicativeAbelian
    , MultiplicativeGroup
    , Distributive
    , Semiring
    , Generic
    )
instance From Natural Probability where
  from :: Natural -> Probability
  from n = Probability (from n)
instance From Rational Probability where
  from :: Rational -> Probability
  from (Ratio p q) = Probability (absolute p `reduce` absolute q)
instance From Probability Rational where
  from :: Probability -> Rational
  from (Probability (Ratio p q)) = from p `reduce` from q
instance From Probability Double where
  from :: Probability -> Double
  from (Probability r) = from r

instance Addition Probability Probability Probability where
  (+.) :: Probability -> Probability -> Probability
  Probability (Ratio m n) +. Probability (Ratio p q) = Probability (reduce (m * q + n * p) (n * q))
instance Multiplication Probability Probability Probability where
  (*.) :: Probability -> Probability -> Probability
  Probability (Ratio m n) *. Probability (Ratio p q) = Probability (reduce (m * p) (n * q))
instance Division Probability Probability Probability where
  (/.) :: Probability -> Probability -> Probability
  Probability (Ratio m n) /. Probability (Ratio p q) = Probability (reduce (m * q) (n * p))
instance Multiplication Rational Probability Rational where
  (*.) :: Rational -> Probability -> Rational
  r *. p = r * from p
instance Multiplication Double Probability Double where
  (*.) :: Double -> Probability -> Double
  d *. p = d * from p

normalize :: [(x, Probability)] -> [(x, Probability)]
normalize ps =
  let !tot = sumOn snd ps
   in morphism (\(x, p) -> (x, p / tot)) ps

shrink :: (Ord x) => [(x, Probability)] -> [(x, Probability)]
shrink ps = normalize do
  morphism
    (\xps@((x, _) :| _) -> (x, sumOn snd xps))
    (List1.groupAllWith fst ps)

newtype Distribution x = Distribution
  {distribution :: [(x, Probability)]}
  deriving (Eq, Ord, Data.Functor)
instance Control.Applicative Distribution where
  pure :: x -> Distribution x
  pure x = Distribution [(x, one)]
  liftA2 :: (x -> y -> z) -> Distribution x -> Distribution y -> Distribution z
  liftA2 (•) (Distribution d0) (Distribution d1) = Distribution do
    normalize [(_0 • _1, p0 * p1) | (_0, p0) <- d0, (_1, p1) <- d1]
instance Control.Monad Distribution where
  (>>=) :: Distribution x -> (x -> Distribution y) -> Distribution y
  Distribution d >>= f = Distribution do
    normalize [(y, px * py) | (x, px) <- d, (y, py) <- distribution (f x)]
instance (Ord x, Show x) => Show (Distribution x) where
  show :: (Ord x, Show x) => Distribution x -> String
  show (Distribution (normalize . shrink -> d)) = List.unlines do
    flip foldWith d \(x, p) ->
      [ padWith xMax ' ' x
          <> " | "
          <> printf "%.4f" (from @_ @Double p)
      ]
   where
    xMax = List1.withList1 0 (List1.maximumOf (List.genericLength . show . fst)) d
    padWith :: (Show x) => Natural -> Char -> x -> String
    padWith n c x = paddedWith n c (show x)
    paddedWith :: Natural -> Char -> String -> String
    paddedWith n c x =
      let nx = from n - length x
       in List.replicate (from nx) c <> x

instance Morphisms (->) (->) Distribution where
  morphism :: (x -> y) -> Distribution x -> Distribution y
  morphism = Data.fmap
instance Pure Distribution where
  pure :: x -> Distribution x
  pure x = Distribution [(x, one)]
instance Apply Distribution where
  liftA2 :: (x -> y -> z) -> Distribution x -> Distribution y -> Distribution z
  liftA2 (•) (Distribution d0) (Distribution d1) = Distribution do
    normalize [(_0 • _1, p0 * p1) | (_0, p0) <- d0, (_1, p1) <- d1]
instance Bind Distribution where
  (>>=) :: Distribution x -> (x -> Distribution y) -> Distribution y
  (>>=) = (Control.>>=)

foldD :: (Ord x) => (x -> x -> x) -> NonEmpty (Distribution x) -> Distribution x
foldD f = foldl1' \(Distribution d) -> liftA2 f (Distribution (shrink d))

likelihood :: (x -> Bool) -> Distribution x -> Probability
likelihood predicate (Distribution d) = sumOn snd (List.filter (predicate . fst) d)

conditional :: (x -> Bool) -> Distribution x -> Distribution x
conditional predicate (Distribution d) = Distribution (normalize (List.filter (predicate . fst) d))

uniform :: (Ord x) => [x] -> Distribution x
uniform = Distribution . normalize . shrink . foldWith \x -> [(x, one)]

fairness :: Probability -> x -> x -> Distribution x
fairness (Probability (Ratio p q)) heads tails = Distribution do
  [(heads, Probability (reduce p q)), (tails, Probability (reduce (q - p) q))]

coin :: Probability -> Distribution Bool
coin p = fairness p True False

binomial :: Natural -> Probability -> Distribution Natural
binomial n p =
  let ds = List1.replicate (from n) (fairness p one zero)
      Distribution d = foldD (+) ds
   in Distribution (shrink d)

die :: Natural -> Distribution Natural
die n = uniform [one .. n]

expectation ::
  (Ord x, AdditiveAbelian x, Multiplication x Probability x) =>
  Distribution x -> x
expectation (Distribution d) = sumOn (uncurry (*.)) (shrink d)

variance ::
  ( Ord x
  , AdditiveAbelian x
  , Subtraction x x x
  , Multiplication x x x
  , Multiplication x Probability x
  ) =>
  Distribution x -> x
variance (Distribution d) = expectation d2 - (e * e)
 where
  d2 = Distribution (shrink (morphism (\(x, p) -> (x * x, p)) d))
  e = expectation (Distribution d)

deviation ::
  (Ord x, Root x, Multiplication x Probability x) =>
  Distribution x -> x
deviation d = 2 √ variance d

quantile :: (Ord x) => Probability -> Distribution x -> Maybe x
quantile p (Distribution d) =
  let dist = shrink d
      cumul = List1.has01 dist [] do
        List1.toList . List1.scanl1' \(_, q') (x, q) -> (x, q' + q)
      r = max zero (min one p)
   in case List.dropWhile ((< r) . snd) cumul of
        [] -> Nothing
        (x, _) : _ -> Just x
