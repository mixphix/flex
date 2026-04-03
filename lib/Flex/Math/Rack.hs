module Flex.Math.Rack
  ( Rack ((<|), (|>))
  , Quandle
  ) where

class Rack x where
  (<|) :: x -> x -> x
  (|>) :: x -> x -> x

class (Rack x) => Quandle x
