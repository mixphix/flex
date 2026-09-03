{-# LANGUAGE UndecidableInstances #-}

module Flex.Math.Action
  ( Action ((@))
  ) where

import Flex.Math.Category
import Flex.Math.Matrix
import Flex.Math.Numbers
import Flex.Math.Permutation

import Data.Semigroup

class Action g x where
  (@) :: g -> x -> x

instance Action (x -> x) x where
  (@) :: (x -> x) -> x -> x
  (@) = id
instance Action (Endo x) x where
  (@) :: Endo x -> x -> x
  (@) = appEndo
instance
  ( KnownNat n
  , AdditiveAbelian x
  , Multiplication x x x
  ) =>
  Action (M n n x) (V n x)
  where
  (@) :: M n n x -> V n x -> V n x
  (@) = (*.)
instance (KnownNat n) => Action (S n) (V n x) where
  (@) :: S n -> V n x -> V n x
  (@) = permute
