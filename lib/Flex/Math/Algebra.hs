{-# LANGUAGE UndecidableInstances #-}

module Flex.Math.Algebra
  ( Algebra
  , Unital
  , AssociativeAlgebra
  , CommutativeAlgebra
  , DivisionAlgebra
  , Signature (..)
  , Laws (..)
  ) where

import Flex.Math.Category
import Flex.Math.Module
import Flex.Math.Numbers
import Flex.Math.Structure

import Data.Bool (Bool)
import Data.Eq (Eq (..))
import Data.List1 (List1)
import GHC.Generics (Generic)
import GHC.Show (Show)

class (Module v, Distributive v) => Algebra v
instance Structure Algebra where
  data Signature Algebra v
    = AlgebraModule (Signature Module v)
    | AlgebraDistributive (Signature Distributive v)
    deriving (Generic)
  data Term Algebra v
    = TermAlgebra v
    | TermAlgebraScalar (Scalar v)
  operations :: (Algebra v) => Signature Algebra v -> Term Algebra v
  operations = \case
    AlgebraModule sig -> case operations sig of
      TermModule op -> TermAlgebra op
      TermModuleScalar op -> TermAlgebraScalar op
    AlgebraDistributive sig -> case operations sig of
      TermDistributive op -> TermAlgebra op
  type Requirements Algebra = C2 Eq (CC Eq Scalar)
  data Laws Algebra v
    = AlgebraModuleLaws (Laws Module v)
    | AlgebraDistributiveLaws (Laws Distributive v)
    | AlgebraMultiplyScalars (Scalar v) v (Scalar v) v
    deriving (Generic)
  lawful ::
    forall v.
    (Algebra v, Requirements Algebra v) =>
    Laws Algebra v -> Bool
  lawful = \case
    AlgebraModuleLaws laws -> lawful laws
    AlgebraDistributiveLaws laws -> lawful laws
    AlgebraMultiplyScalars a x b y ->
      let (+*) = (*.) @(Scalar v) @v @v
       in (a +* x) * (b +* y) == (a * b) +* (x * y)
deriving instance (Show v, Show (Scalar v)) => Show (Signature Algebra v)
deriving instance (Show v, Show (Scalar v)) => Show (Laws Algebra v)

instance (Eq x, Field x) => Algebra (List1 x)
instance (Eq x, Field x) => Algebra (Complex x)
instance (Eq x, Field x) => Algebra (Quaternion x)

class (Algebra v) => Unital v
instance Structure Unital where
  data Signature Unital v
    = UnitalAlgebra (Signature Algebra v)
    deriving (Generic)
  data Term Unital v
    = TermUnital v
    | TermUnitalScalar (Scalar v)
  operations :: (Unital v) => Signature Unital v -> Term Unital v
  operations = \case
    UnitalAlgebra sig -> case operations sig of
      TermAlgebra op -> TermUnital op
      TermAlgebraScalar op -> TermUnitalScalar op
  type Requirements Unital = C2 Eq (CC Eq Scalar)
  data Laws Unital v
    = UnitalAlgebraLaws (Laws Algebra v)
    | UnitalOneLeft v
    | UnitalOneRight v
    deriving (Generic)
  lawful ::
    forall v.
    (Unital v, Requirements Unital v) =>
    Laws Unital v -> Bool
  lawful = \case
    UnitalAlgebraLaws laws -> lawful laws
    UnitalOneLeft v -> one @v * v == v
    UnitalOneRight v -> v * one @v == v
deriving instance (Show v, Show (Scalar v)) => Show (Signature Unital v)
deriving instance (Show v, Show (Scalar v)) => Show (Laws Unital v)

instance (Eq x, Field x) => Unital (List1 x)
instance (Eq x, Field x) => Unital (Complex x)
instance (Eq x, Field x) => Unital (Quaternion x)

class (Unital v) => AssociativeAlgebra v
instance Structure AssociativeAlgebra where
  data Signature AssociativeAlgebra v
    = AssociativeAlgebraUnital (Signature Unital v)
    deriving (Generic)
  data Term AssociativeAlgebra v
    = TermAssociativeAlgebra v
    | TermAssociativeAlgebraScalar (Scalar v)
  operations ::
    (AssociativeAlgebra v) =>
    Signature AssociativeAlgebra v ->
    Term AssociativeAlgebra v
  operations = \case
    AssociativeAlgebraUnital sig -> case operations sig of
      TermUnital op -> TermAssociativeAlgebra op
      TermUnitalScalar op -> TermAssociativeAlgebraScalar op
  type Requirements AssociativeAlgebra = C2 Eq (CC Eq Scalar)
  data Laws AssociativeAlgebra v
    = AssociativeAlgebraUnitalLaws (Laws Unital v)
    | AssociativeAlgebraAssociative v v v
    deriving (Generic)
  lawful ::
    forall v.
    (AssociativeAlgebra v, Requirements AssociativeAlgebra v) =>
    Laws AssociativeAlgebra v -> Bool
  lawful = \case
    AssociativeAlgebraUnitalLaws laws -> lawful laws
    AssociativeAlgebraAssociative v w x -> v * (w * x) == (v * w) * x
deriving instance
  (Show v, Show (Scalar v)) => Show (Signature AssociativeAlgebra v)
deriving instance (Show v, Show (Scalar v)) => Show (Laws AssociativeAlgebra v)

instance (Eq x, Field x) => AssociativeAlgebra (List1 x)
instance (Eq x, Field x) => AssociativeAlgebra (Complex x)
instance (Eq x, Field x) => AssociativeAlgebra (Quaternion x)

class (MultiplicativeAbelian (Scalar v), Unital v) => CommutativeAlgebra v
instance Structure CommutativeAlgebra where
  data Signature CommutativeAlgebra v
    = CommutativeAlgebraUnital (Signature Unital v)
    deriving (Generic)
  data Term CommutativeAlgebra v
    = TermCommutativeAlgebra v
    | TermCommutativeAlgebraScalar (Scalar v)
  operations ::
    (CommutativeAlgebra v) =>
    Signature CommutativeAlgebra v ->
    Term CommutativeAlgebra v
  operations = \case
    CommutativeAlgebraUnital sig -> case operations sig of
      TermUnital op -> TermCommutativeAlgebra op
      TermUnitalScalar op -> TermCommutativeAlgebraScalar op
  type Requirements CommutativeAlgebra = C2 Eq (CC Eq Scalar)
  data Laws CommutativeAlgebra v
    = CommutativeAlgebraUnitalLaws (Laws Unital v)
    | CommutativeAlgebraCommutative v v
    deriving (Generic)
  lawful ::
    forall v.
    (CommutativeAlgebra v, Requirements CommutativeAlgebra v) =>
    Laws CommutativeAlgebra v -> Bool
  lawful = \case
    CommutativeAlgebraUnitalLaws laws -> lawful laws
    CommutativeAlgebraCommutative v w -> v * w == w * v
deriving instance
  (Show v, Show (Scalar v)) => Show (Signature CommutativeAlgebra v)
deriving instance (Show v, Show (Scalar v)) => Show (Laws CommutativeAlgebra v)

instance (Eq x, Field x) => CommutativeAlgebra (List1 x)
instance (Eq x, Field x) => CommutativeAlgebra (Complex x)

class (Unital v, MultiplicativeGroup v) => DivisionAlgebra v
instance Structure DivisionAlgebra where
  data Signature DivisionAlgebra v
    = DivisionAlgebraUnital (Signature Unital v)
    | DivisionAlgebraMultiplicativeGroup (Signature MultiplicativeGroup v)
    deriving (Generic)
  data Term DivisionAlgebra v
    = TermDivisionAlgebra v
    | TermDivisionAlgebraScalar (Scalar v)
  operations ::
    (DivisionAlgebra v) =>
    Signature DivisionAlgebra v ->
    Term DivisionAlgebra v
  operations = \case
    DivisionAlgebraUnital sig -> case operations sig of
      TermUnital op -> TermDivisionAlgebra op
      TermUnitalScalar op -> TermDivisionAlgebraScalar op
    DivisionAlgebraMultiplicativeGroup sig -> case operations sig of
      TermMultiplicativeGroup op -> TermDivisionAlgebra op
  type Requirements DivisionAlgebra = C2 Eq (CC Eq Scalar)
  data Laws DivisionAlgebra v
    = DivisionAlgebraUnitalLaws (Laws Unital v)
    | DivisionAlgebraMultiplicativeGroupLaws (Laws MultiplicativeGroup v)
    deriving (Generic)
  lawful ::
    forall v.
    (DivisionAlgebra v, Requirements DivisionAlgebra v) =>
    Laws DivisionAlgebra v -> Bool
  lawful = \case
    DivisionAlgebraUnitalLaws laws -> lawful laws
    DivisionAlgebraMultiplicativeGroupLaws laws -> lawful laws
deriving instance
  (Show v, Show (Scalar v)) => Show (Signature DivisionAlgebra v)
deriving instance (Show v, Show (Scalar v)) => Show (Laws DivisionAlgebra v)

instance (Eq x, Field x) => DivisionAlgebra (Complex x)
instance (Eq x, Field x, Conjugate x) => DivisionAlgebra (Quaternion x)
