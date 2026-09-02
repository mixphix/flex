{-# LANGUAGE ViewPatterns #-}

module Flex.Math.Basis
  ( Basis (basis)
  ) where

import Flex.Math.Category
import Flex.Math.Dual
import Flex.Math.Matrix
import Flex.Math.Minkowski
import Flex.Math.Module
import Flex.Math.Numbers
import Flex.Math.Perplex

import Data.Eq (Eq ((==)))
import Data.Finite (Finite, getFinite)
import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, natVal)

class (Module (v x), Tabulation v) => Basis v x where
  basis :: Table v -> v x

instance (KnownNat n, Ring x) => Basis (V n) x where
  basis :: Finite n -> V n x
  basis (from . getFinite -> j) =
    vn (natVal (Proxy @n)) \i -> if i == j then one else zero

instance (Ring x) => Basis Complex x where
  basis :: ComplexBasis -> Complex x
  basis = \case
    Real -> one :+ zero
    Imaginary -> zero :+ one

instance (Ring x) => Basis Quaternion x where
  basis :: QuaternionBasis -> Quaternion x
  basis = \case
    E -> Quaternion one zero zero zero
    I -> Quaternion zero one zero zero
    J -> Quaternion zero zero one zero
    K -> Quaternion zero zero zero one

instance (Ring x) => Basis Dual x where
  basis :: DualBasis -> Dual x
  basis = \case
    Primal -> one :& zero
    Dual -> zero :& one

instance (Ring x) => Basis Perplex x where
  basis :: PerplexBasis -> Perplex x
  basis = \case
    Simple -> one :! zero
    Perplex -> zero :! one

instance (Ring x) => Basis Minkowski x where
  basis :: MinkowskiBasis -> Minkowski x
  basis = \case
    T -> Minkowski one zero zero zero
    X -> Minkowski zero one zero zero
    Y -> Minkowski zero zero one zero
    Z -> Minkowski zero zero zero one
