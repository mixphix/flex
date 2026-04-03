module Flex.Math.Lattice
  ( Meet ((/\))
  , Lowest (lowest)
  , Join ((\/))
  , Highest (highest)
  , Lattice
  , Extrema
  , Heyting ((-->), complement, xor)
  , Boolean
  , Median (median)
  ) where

import Flex.Math.Suspension

import Data.Bool (Bool (..), not, (&&), (||))
import Data.Ord (Ord)

class (Ord x) => Meet x where
  (/\) :: x -> x -> x
class (Meet x) => Lowest x where
  lowest :: x
class (Ord x) => Join x where
  (\/) :: x -> x -> x
class (Join x) => Highest x where
  highest :: x
class (Meet x, Join x) => Lattice x
class (Lowest x, Highest x) => Extrema x
class (Extrema x) => Heyting x where
  (-->) :: x -> x -> x

  complement :: x -> x
  complement x = x --> lowest

  xor :: x -> x -> x
  xor p q = (p \/ q) /\ complement (p /\ q)

class (Heyting x) => Boolean x
class (Boolean x) => Median x where
  median :: x -> x -> x -> x
  median x y z = (x \/ y) /\ (y \/ z) /\ (z \/ x)

instance Meet Bool where
  (/\) :: Bool -> Bool -> Bool
  (/\) = (&&)
instance Lowest Bool where
  lowest :: Bool
  lowest = False
instance Join Bool where
  (\/) :: Bool -> Bool -> Bool
  (\/) = (||)
instance Highest Bool where
  highest :: Bool
  highest = True
instance Lattice Bool
instance Extrema Bool
instance Heyting Bool where
  (-->) :: Bool -> Bool -> Bool
  x --> y = not x || y
  complement :: Bool -> Bool
  complement = not
  xor :: Bool -> Bool -> Bool
  xor = \cases
    False False -> False
    True False -> True
    False True -> True
    True True -> False
instance Boolean Bool
instance Median Bool

instance (Meet x) => Meet (Suspension x) where
  (/\) :: Suspension x -> Suspension x -> Suspension x
  South /\ _ = South
  _ /\ South = South
  North /\ x = x
  x /\ North = x
  Meridian x /\ Meridian y = Meridian (x /\ y)

instance (Join x) => Join (Suspension x) where
  (\/) :: Suspension x -> Suspension x -> Suspension x
  South \/ x = x
  x \/ South = x
  North \/ _ = North
  _ \/ North = North
  Meridian x \/ Meridian y = Meridian (x \/ y)

instance (Meet x) => Lowest (Suspension x) where
  lowest :: Suspension x
  lowest = South

instance (Join x) => Highest (Suspension x) where
  highest :: Suspension x
  highest = North

instance (Lattice x) => Lattice (Suspension x)

instance (Lattice x) => Extrema (Suspension x)
