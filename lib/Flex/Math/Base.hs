{-# LANGUAGE UndecidableInstances #-}

module Flex.Math.Base
  ( Base (Base, getBase)
  , base
  , unbase
  , rebase
  , digits
  , undigits
  ) where

import Flex.Math.Category
import Flex.Math.Numbers

import Data.Eq (Eq)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ord (Ord (..))
import Data.Proxy (Proxy (Proxy))
import Data.Type.Ord (type (>))
import GHC.Read (Read)
import GHC.Real (Integral (quotRem))
import GHC.Real qualified as Num
import GHC.Show (Show)
import GHC.TypeNats (KnownNat, Nat, natVal)

newtype Base (n :: Nat) = Base {getBase :: NonEmpty Natural}
  deriving newtype (Eq, Ord, Show, Read)

base :: forall n. (KnownNat n, n > 0) => Natural -> Base n
base 0 = Base (pure 0)
base n = Base case natVal (Proxy @n) of
  b | n < b -> pure n
  b ->
    let (q, r) = n `quotRem` b
        Base ds = base @n q
     in Num.fromIntegral r NonEmpty.<| ds

unbase :: forall n. (KnownNat n, n > 0) => Base n -> Natural
unbase (Base (d :| ds)) =
  let b = natVal (Proxy @n)
   in d + (b * case ds of [] -> 0; c : cs -> unbase (Base @n (c :| cs)))

rebase ::
  forall m n.
  (KnownNat m, KnownNat n, m > 0, n > 0) => Base m -> Base n
rebase = base @n . unbase @m

digits :: Natural -> NonEmpty Natural
digits = getBase . base @10

undigits :: NonEmpty Natural -> Natural
undigits = unbase @10 . Base

instance
  ( KnownNat m
  , m > 0
  , KnownNat n
  , n > 0
  , KnownNat k
  , k > 0
  ) =>
  Addition (Base m) (Base n) (Base k)
  where
  (+.) :: Base m -> Base n -> Base k
  m +. n = base @k (unbase @m m + unbase @n n)

instance
  ( KnownNat m
  , m > 0
  , KnownNat n
  , n > 0
  , KnownNat k
  , k > 0
  ) =>
  Subtraction (Base m) (Base n) (Base k)
  where
  (-.) :: Base m -> Base n -> Base k
  m -. n = base @k (unbase @m m - unbase @n n)

instance
  ( KnownNat m
  , m > 0
  , KnownNat n
  , n > 0
  , KnownNat k
  , k > 0
  ) =>
  Multiplication (Base m) (Base n) (Base k)
  where
  (*.) :: Base m -> Base n -> Base k
  m *. n = base @k (unbase @m m * unbase @n n)
