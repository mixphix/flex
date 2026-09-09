module Flex.Math.Function where

import Flex.Math.Category

import Data.Bool
import Data.Either
import Data.Function

loop :: x -> (x -> Either x y) -> y
loop start with = case with start of
  Left next -> loop next with
  Right y -> y
{-# INLINE loop #-}

loopM :: (Monad m) => x -> (x -> m (Either x y)) -> m y
loopM start with =
  with start >>= \case
    Left next -> loopM next with
    Right y -> pure y
{-# INLINE loopM #-}

while :: (Monad m) => m Bool -> m ()
while = fix \rec condition ->
  condition >>= \truth -> when truth (rec condition)
{-# INLINE while #-}
