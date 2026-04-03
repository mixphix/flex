module Flex.Math.Projective
  ( Projective (Projective, Infinity)
  , projective
  , Tropical (Tropical, Pole)
  , tropical
  ) where

import Flex.Math.Category

import Control.Applicative qualified as Control
import Control.Monad qualified as Control
import Data.Data (Data)
import Data.Eq (Eq)
import Data.Foldable qualified as Data
import Data.Functor qualified as Data
import Data.Monoid (Monoid (mempty))
import Data.Ord (Ord)
import Data.Traversable qualified as Data
import GHC.Generics (Generic)
import GHC.Read (Read)
import GHC.Show (Show)

data Projective x
  = Projective x
  | Infinity
  deriving
    ( Eq
    , Ord
    , Show
    , Read
    , Data.Functor
    , Data.Foldable
    , Data.Traversable
    , Data
    , Generic
    )

instance Morphisms (->) (->) Projective where
  morphism :: (x -> y) -> Projective x -> Projective y
  morphism = Data.fmap
instance Pure Projective where
  pure :: x -> Projective x
  pure = Projective
instance Apply Projective where
  liftA2 :: (x -> y -> z) -> Projective x -> Projective y -> Projective z
  liftA2 xyz = \cases
    (Projective x) (Projective y) -> Projective (xyz x y)
    _ _ -> Infinity
instance Bind Projective where
  (>>=) :: Projective x -> (x -> Projective y) -> Projective y
  (>>=) = \cases
    Infinity _ -> Infinity
    (Projective x) f -> f x
instance Folds (->) (->) Projective where
  foldWith :: (Monoid z) => (x -> z) -> Projective x -> z
  foldWith x_z = \case
    Projective x -> x_z x
    Infinity -> mempty
instance Traversals (->) (->) Projective where
  traverse :: (Applicative g) => (x -> g y) -> Projective x -> g (Projective y)
  traverse x_fy = \case
    Projective x -> morphism Projective (x_fy x)
    Infinity -> pure Infinity
instance Control.Applicative Projective where
  pure :: x -> Projective x
  pure = Projective
  liftA2 :: (x -> y -> z) -> Projective x -> Projective y -> Projective z
  liftA2 xyz = \cases
    (Projective x) (Projective y) -> Projective (xyz x y)
    _ _ -> Infinity
instance Control.Monad Projective where
  (>>=) :: Projective x -> (x -> Projective y) -> Projective y
  (>>=) = \cases
    Infinity _ -> Infinity
    (Projective x) f -> f x

projective :: (x -> y) -> y -> Projective x -> y
projective projective_ infinity = \case
  Projective x -> projective_ x
  Infinity -> infinity

newtype Tropical x = TropicalProjective (Projective x)
  deriving
    ( Eq
    , Show
    , Read
    , Data.Functor
    , Data.Foldable
    , Data.Traversable
    , Data
    , Generic
    )

pattern Tropical :: x -> Tropical x
pattern Tropical x = TropicalProjective (Projective x)

pattern Pole :: Tropical x
pattern Pole = TropicalProjective Infinity

{-# COMPLETE Tropical, Pole #-}

instance Morphisms (->) (->) Tropical where
  morphism :: (x -> y) -> Tropical x -> Tropical y
  morphism = Data.fmap
instance Pure Tropical where
  pure :: x -> Tropical x
  pure = Tropical
instance Apply Tropical where
  liftA2 :: (x -> y -> z) -> Tropical x -> Tropical y -> Tropical z
  liftA2 x_y_z = \cases
    (Tropical x) (Tropical y) -> Tropical (x_y_z x y)
    _ _ -> Pole
instance Bind Tropical where
  (>>=) :: Tropical x -> (x -> Tropical y) -> Tropical y
  (>>=) = \cases
    Pole _ -> Pole
    (Tropical x) f -> f x
instance Folds (->) (->) Tropical where
  foldWith :: (Monoid z) => (x -> z) -> Tropical x -> z
  foldWith x_z = \case
    Tropical x -> x_z x
    Pole -> mempty
instance Traversals (->) (->) Tropical where
  traverse :: (Applicative g) => (x -> g y) -> Tropical x -> g (Tropical y)
  traverse x_fy = \case
    Tropical x -> morphism Tropical (x_fy x)
    Pole -> pure Pole
instance Control.Applicative Tropical where
  pure :: x -> Tropical x
  pure = Tropical
  liftA2 :: (x -> y -> z) -> Tropical x -> Tropical y -> Tropical z
  liftA2 = liftA2
instance Control.Monad Tropical where
  (>>=) :: Tropical x -> (x -> Tropical y) -> Tropical y
  (>>=) = \cases
    Pole _ -> Pole
    (Tropical x) f -> f x

tropical :: (x -> y) -> y -> Tropical x -> y
tropical projective_ infinity = \case
  TropicalProjective p -> projective projective_ infinity p
