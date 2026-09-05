module Flex.Math.Permutation
  ( S (s)
  , permute
  , transposition
  ) where

import Flex.Math.Category
import Flex.Math.Matrix
import Flex.Math.Numbers

import Data.Bool
import Data.Bounded (Bounded (..))
import Data.Enum (Enum (..))
import Data.Eq (Eq ((==)))
import Data.List qualified as List
import Data.Maybe
import Data.Ord (Ord)
import Data.Tuple
import Data.Vector qualified as Vector
import Text.Show (Show)

newtype S n = S {s :: V n (Finite n)}
  deriving newtype
    ( Eq
    , Ord
    , Show
    )

instance (KnownNat n) => Semigroup (S n) where
  (<>) :: S n -> S n -> S n
  (<>) = (+.)
instance (KnownNat n) => Monoid (S n) where
  mempty :: S n
  mempty = zero

instance (KnownNat n) => Addition (S n) (S n) (S n) where
  (+.) :: S n -> S n -> S n
  S s +. s' = S (permute s' s)
instance (KnownNat n) => Additive (S n) where
  zero :: S n
  zero = S (V (Vector.generate (from (natVal (Proxy @n))) from))
instance (KnownNat n) => Subtraction (S n) (S n) (S n) where
  (-.) :: S n -> S n -> S n
  s -. s' = s + negative s'
instance (KnownNat n) => AdditiveGroup (S n) where
  negative :: S n -> S n
  negative (S s) = S do
    let S ø = zero @(S n)
        v = toList (zip s ø)
     in V (Vector.fromList (morphism snd (List.sortOn fst v)))

permute :: (KnownNat n) => S n -> V n x -> V n x
permute (S s) (V u) = V do
  Vector.backpermute u (morphism from s.unV)

transposition :: forall n. (KnownNat n) => Finite n -> S n
transposition f
  | f == maxBound = S do
      vn (natVal (Proxy @n)) \case
        i
          | Just ix <- i `List.elemIndex` [from f, zero] -> case ix of
              0 -> from (zero @Natural)
              _ -> f
          | otherwise -> from i
  | otherwise = S do
      vn (natVal (Proxy @n)) \case
        i
          | Just ix <- i `List.elemIndex` [from f, succ (from f)] -> case ix of
              0 -> succ f
              _ -> f
          | otherwise -> from i
