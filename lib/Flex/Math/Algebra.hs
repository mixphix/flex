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
import Flex.Math.Variety

import Data.Bool (Bool)
import Data.Eq (Eq (..))
import Data.List1 (List1)
import GHC.Generics (Generic)
import GHC.Show (Show)

class (Module v, Distributive v) => Algebra v
instance Variety Algebra where
  type Requirements Algebra = C2 Eq (CC Eq Scalar)
  data Signature Algebra v
    = AlgebraModule (Signature Module v)
    | AlgebraDistributive (Signature Distributive v)
    deriving (Generic)
  data Operations Algebra v
    = OperationsAlgebra v
    | OperationsAlgebraScalar (Scalar v)
  operations :: (Algebra v) => Signature Algebra v -> Operations Algebra v
  operations = \case
    AlgebraModule sig -> case operations sig of
      OperationsModule op -> OperationsAlgebra op
      OperationsModuleScalar op -> OperationsAlgebraScalar op
    AlgebraDistributive sig -> case operations sig of
      OperationsDistributive op -> OperationsAlgebra op
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
instance Variety Unital where
  type Requirements Unital = C2 Eq (CC Eq Scalar)
  data Signature Unital v
    = UnitalAlgebra (Signature Algebra v)
    deriving (Generic)
  data Operations Unital v
    = OperationsUnital v
    | OperationsUnitalScalar (Scalar v)
  operations :: (Unital v) => Signature Unital v -> Operations Unital v
  operations = \case
    UnitalAlgebra sig -> case operations sig of
      OperationsAlgebra op -> OperationsUnital op
      OperationsAlgebraScalar op -> OperationsUnitalScalar op
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
instance Variety AssociativeAlgebra where
  type Requirements AssociativeAlgebra = C2 Eq (CC Eq Scalar)
  data Signature AssociativeAlgebra v
    = AssociativeAlgebraUnital (Signature Unital v)
    deriving (Generic)
  data Operations AssociativeAlgebra v
    = OperationsAssociativeAlgebra v
    | OperationsAssociativeAlgebraScalar (Scalar v)
  operations ::
    (AssociativeAlgebra v) =>
    Signature AssociativeAlgebra v ->
    Operations AssociativeAlgebra v
  operations = \case
    AssociativeAlgebraUnital sig -> case operations sig of
      OperationsUnital op -> OperationsAssociativeAlgebra op
      OperationsUnitalScalar op -> OperationsAssociativeAlgebraScalar op
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
instance Variety CommutativeAlgebra where
  type Requirements CommutativeAlgebra = C2 Eq (CC Eq Scalar)
  data Signature CommutativeAlgebra v
    = CommutativeAlgebraUnital (Signature Unital v)
    deriving (Generic)
  data Operations CommutativeAlgebra v
    = OperationsCommutativeAlgebra v
    | OperationsCommutativeAlgebraScalar (Scalar v)
  operations ::
    (CommutativeAlgebra v) =>
    Signature CommutativeAlgebra v ->
    Operations CommutativeAlgebra v
  operations = \case
    CommutativeAlgebraUnital sig -> case operations sig of
      OperationsUnital op -> OperationsCommutativeAlgebra op
      OperationsUnitalScalar op -> OperationsCommutativeAlgebraScalar op
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
instance Variety DivisionAlgebra where
  type Requirements DivisionAlgebra = C2 Eq (CC Eq Scalar)
  data Signature DivisionAlgebra v
    = DivisionAlgebraUnital (Signature Unital v)
    | DivisionAlgebraMultiplicativeGroup (Signature MultiplicativeGroup v)
    deriving (Generic)
  data Operations DivisionAlgebra v
    = OperationsDivisionAlgebra v
    | OperationsDivisionAlgebraScalar (Scalar v)
  operations ::
    (DivisionAlgebra v) =>
    Signature DivisionAlgebra v ->
    Operations DivisionAlgebra v
  operations = \case
    DivisionAlgebraUnital sig -> case operations sig of
      OperationsUnital op -> OperationsDivisionAlgebra op
      OperationsUnitalScalar op -> OperationsDivisionAlgebraScalar op
    DivisionAlgebraMultiplicativeGroup sig -> case operations sig of
      OperationsMultiplicativeGroup op -> OperationsDivisionAlgebra op
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
