module Flex.Math.Rack
  ( Rack ((<|), (|>))
  , Quandle
  ) where

-- |
-- A 'Rack' is a type of algebraic structure with two actions,
-- '(<|)' and '(|>)', such that:
--
-- @
-- forall x y z.
--   x <| (y <| z) == (x <| y) <| (x <| z),
--   (x |> y) |> z == (x |> z) |> (y <| z),
--   (x \<| y) |> x == y,
--   x \<| (y |> x) == y.
-- @
class Rack x where
  (<|) :: x -> x -> x
  (|>) :: x -> x -> x

instance (Rack x, Rack y) => Rack (x, y) where
  (<|) :: (x, y) -> (x, y) -> (x, y)
  (x, y) <| (x', y') = (x <| x', y <| y')
  {-# INLINE (<|) #-}
  (|>) :: (x, y) -> (x, y) -> (x, y)
  (x, y) |> (x', y') = (x |> x', y |> y')
  {-# INLINE (|>) #-}

-- |
-- A 'Quandle' is a 'Rack' such that
--
-- @
-- forall x. (x <| x).
-- @
class (Rack x) => Quandle x
instance (Quandle x, Quandle y) => Quandle (x, y)
