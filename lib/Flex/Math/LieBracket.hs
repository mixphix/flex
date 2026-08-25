{-# LANGUAGE UndecidableInstances #-}

module Flex.Math.LieBracket
  ( LieBracket ((><))
  , Signature (..)
  , Laws (..)
  ) where

import Flex.Math.Matrix
import Flex.Math.Module
import Flex.Math.Numbers
import Flex.Math.Structure

import Data.Bool (Bool)
import Data.Eq (Eq (..))
import GHC.Generics (Generic)
import GHC.Show (Show)

class (Module v) => LieBracket v where
  (><) :: v -> v -> v
instance Structure LieBracket where
  data Signature LieBracket v
    = LieBracketAdditiveGroup (Signature AdditiveGroup v)
    | LieBracketMultiply v v
    deriving (Generic)
  newtype Term LieBracket v = TermLieBracket v
  operations ::
    (LieBracket v) =>
    Signature LieBracket v ->
    Term LieBracket v
  operations = \case
    LieBracketAdditiveGroup sig -> case operations sig of
      TermAdditiveGroup op -> TermLieBracket op
    LieBracketMultiply x y -> TermLieBracket (x >< y)
  type Requirements LieBracket = Eq
  data Laws LieBracket v
    = LieBracketAdditiveGroupLaws (Laws AdditiveGroup v)
    | LieBracketLinearFirst (Scalar v) v (Scalar v) v v
    | LieBracketLinearSecond v (Scalar v) v (Scalar v) v
    | LieBracketAlternating v
    | LieBracketJacobi v v v
    deriving (Generic)
  lawful :: forall v. (LieBracket v, Eq v) => Laws LieBracket v -> Bool
  lawful = \case
    LieBracketAdditiveGroupLaws laws -> lawful laws
    LieBracketLinearFirst a x b y z ->
      let (+*) = (*.) @(Scalar v) @v @v
       in (a +* x + b +* y) >< z == a +* (x >< z) + b +* (y >< z)
    LieBracketLinearSecond x a y b z ->
      let (+*) = (*.) @(Scalar v) @v @v
       in x >< (a +* y + b +* z) == a +* (x >< y) + b +* (x >< z)
    LieBracketAlternating x -> x >< x == zero @v
    LieBracketJacobi x y z ->
      (x >< (y >< z)) + (y >< (z >< x)) + (z >< (x >< y)) == zero @v
deriving instance (Show v, Show (Scalar v)) => Show (Signature LieBracket v)
deriving instance (Show v, Show (Scalar v)) => Show (Laws LieBracket v)

instance {-# OVERLAPPABLE #-} (Module v, Multiplication v v v) => LieBracket v where
  (><) :: v -> v -> v
  a >< b = (a * b) - (b * a)

instance {-# OVERLAPPING #-} (Eq x, Ring x) => LieBracket (V 3 x) where
  (><) :: V 3 x -> V 3 x -> V 3 x
  a >< b = vn 3 \case
    0 -> (a ! 1 * b ! 2) - (a ! 2 * b ! 1)
    1 -> (a ! 2 * b ! 0) - (a ! 0 * b ! 2)
    _ -> (a ! 0 * b ! 1) - (a ! 1 * b ! 0)
