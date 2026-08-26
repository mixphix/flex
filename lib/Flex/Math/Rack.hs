module Flex.Math.Rack
  ( Rack ((<|), (|>))
  , Quandle
  ) where

class Rack x where
  (<|) :: x -> x -> x
  (|>) :: x -> x -> x

instance (Rack x, Rack y) => Rack (x, y) where
  (<|) :: (x, y) -> (x, y) -> (x, y)
  (x, y) <| (x', y') = (x <| x', y <| y')
  (|>) :: (x, y) -> (x, y) -> (x, y)
  (x, y) |> (x', y') = (x |> x', y |> y')

class (Rack x) => Quandle x
instance (Quandle x, Quandle y) => Quandle (x, y)
