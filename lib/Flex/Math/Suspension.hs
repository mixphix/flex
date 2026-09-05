module Flex.Math.Suspension
  ( Suspension (South, Meridian, North)
  , suspension
  ) where

import Flex.Math.Category

import Control.Applicative qualified as Control
import Control.Monad qualified as Control
import Data.Data (Data)
import Data.Eq (Eq)
import Data.Foldable qualified as Data
import Data.Functor qualified as Data
import Data.Ord (Ord)
import Data.Traversable qualified as Data
import GHC.Generics (Generic)
import GHC.Read (Read)
import GHC.Show (Show)

data Suspension x
  = South
  | Meridian x
  | North
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

instance Morphisms (->) (->) Suspension where
  morphism :: (x -> y) -> Suspension x -> Suspension y
  morphism = Data.fmap
instance Pure Suspension where
  pure :: x -> Suspension x
  pure = Meridian
instance Apply Suspension where
  (<*>) :: Suspension (x -> y) -> Suspension x -> Suspension y
  (<*>) = \cases
    South _ -> South
    (Meridian _) South -> South
    (Meridian f) (Meridian x) -> Meridian (f x)
    (Meridian _) North -> North
    North _ -> North
instance Bind Suspension where
  (>>=) :: Suspension x -> (x -> Suspension y) -> Suspension y
  (>>=) = \cases
    South _ -> South
    (Meridian x) f -> f x
    North _ -> North
instance Folds (->) (->) Suspension where
  foldWith :: (Monoid z) => (x -> z) -> Suspension x -> z
  foldWith x_z = \case
    South -> mempty
    Meridian x -> x_z x
    North -> mempty
instance Traversals (->) (->) Suspension where
  traverse :: (Applicative g) => (x -> g y) -> Suspension x -> g (Suspension y)
  traverse x_gy = \case
    South -> pure South
    Meridian x -> morphism Meridian (x_gy x)
    North -> pure North

suspension :: y -> (x -> y) -> y -> Suspension x -> y
suspension south meridian north = \case
  South -> south
  Meridian x -> meridian x
  North -> north

instance Control.Applicative Suspension where
  pure :: x -> Suspension x
  pure = Meridian
  (<*>) :: Suspension (x -> y) -> Suspension x -> Suspension y
  (<*>) = \cases
    South _ -> South
    (Meridian _) South -> South
    (Meridian f) (Meridian x) -> Meridian (f x)
    (Meridian _) North -> North
    North _ -> North
instance Control.Monad Suspension where
  (>>=) :: Suspension x -> (x -> Suspension y) -> Suspension y
  (>>=) = \cases
    South _ -> South
    (Meridian x) f -> f x
    North _ -> North
