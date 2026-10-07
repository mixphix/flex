module Flex.Math.Function
  ( loop
  , loopM
  , while
  , mergeSorted
  , mergeSorted1
  , mergeSortedBy
  ) where

import Flex.Math.Category

import Data.Bool
import Data.Either
import Data.Function
import Data.List (List)
import Data.List.NonEmpty (data (:|))
import Data.List1 (List1)
import Data.List1 qualified as List1
import Data.Ord
import GHC.Err (error)

-- |
-- Run a function on a value until it's 'Right'.
loop :: x -> (x -> Either x y) -> y
loop start with = case with start of
  Left next -> loop next with
  Right y -> y
{-# INLINE loop #-}

-- |
-- Run a monadic function on a value until it's 'Right'.
loopM :: (Monad m) => x -> (x -> m (Either x y)) -> m y
loopM start with =
  with start >>= \case
    Left next -> loopM next with
    Right y -> pure y
{-# INLINE loopM #-}

-- |
-- Run a monadic action until it's 'False'.
while :: (Monad m) => m Bool -> m ()
while = fix \rec condition ->
  condition >>= \truth -> when truth (rec condition)
{-# INLINE while #-}

mergeSortedBy :: (x -> x -> Ordering) -> List x -> List x -> List x
mergeSortedBy (<=>) = fix \rec -> \cases
  [] ys -> ys
  xs [] -> xs
  xs@(x : xs') ys@(y : ys') -> case x <=> y of
    LT -> x : rec xs' ys
    EQ -> x : rec xs' ys
    GT -> y : rec xs ys'

mergeSorted :: (Ord x) => List x -> List x -> List x
mergeSorted = mergeSortedBy compare

mergeSorted1 :: (Ord x) => List1 x -> List1 x -> List1 x
mergeSorted1 xs ys = case mergeSorted (List1.toList xs) (List1.toList ys) of
  [] -> error "mergeSorted1: empty list"
  z : zs -> z :| zs
