{-# OPTIONS_GHC -Wno-duplicate-exports #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Flex.Math.Variety
  ( Variety (Requirements, Signature, Operations, operations, Laws, lawful)
  , EqLaw (EqLawReflexive, EqLawSymmetric, EqLawTransitive)
  , OrdLaw (OrdLawReflexive, OrdLawAntisymmetric, OrdLawTransitive)
  , Signature (..)
  , Operations (..)
  , Laws (..)
  ) where

import Flex.Math.Category
import Flex.Math.Lattice
import Flex.Math.Numbers
import Flex.Math.Rack

import Data.Bool (Bool, (&&), (||))
import Data.Eq (Eq (..))
import Data.Functor.Classes (Eq1)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Constraint, Type)
import Data.Monoid (Monoid (mempty))
import Data.Ord (Ord (..))
import Data.Semigroup (Semigroup ((<>)))
import GHC.Generics (Generic)
import GHC.Show (Show)
import Data.Proxy (Proxy (..))

class Variety (var :: k -> Constraint) where
  type Requirements var :: k -> Constraint
  data Signature var :: k -> Type
  data Operations var :: k -> Type
  operations :: (var x) => Signature var x -> Operations var x
  data Laws var :: k -> Type
  lawful :: (var x, Requirements var x) => Laws var x -> Bool

data EqLaw x
  = EqLawReflexive x
  | EqLawSymmetric x x
  | EqLawTransitive x x x
  deriving (Eq, Ord, Show, Generic)

instance Variety Eq where
  type Requirements Eq = C0
  data Signature Eq x deriving (Generic)
  data Operations Eq x
  operations :: (Eq x) => Signature Eq x -> Operations Eq x
  operations = \case {}
  newtype Laws Eq x = EqLaw (EqLaw x)
    deriving (Show, Generic)
  lawful :: (Eq x) => Laws Eq x -> Bool
  lawful = \case
    EqLaw (EqLawReflexive x) -> x == x
    EqLaw (EqLawSymmetric x y) -> (x == y) --> (y == x)
    EqLaw (EqLawTransitive x y z) -> (x == y && y == z) --> (x == z)

data OrdLaw x
  = OrdLawReflexive x
  | OrdLawAntisymmetric x x
  | OrdLawTransitive x x x
  deriving (Eq, Ord, Show, Generic)

instance Variety Ord where
  type Requirements Ord = C0
  data Signature Ord x deriving (Generic)
  data Operations Ord x
  operations :: (Ord x) => Signature Ord x -> Operations Ord x
  operations = \case {}
  newtype Laws Ord x = OrdLaw (OrdLaw x)
    deriving (Show, Generic)
  lawful :: (Ord x) => Laws Ord x -> Bool
  lawful = \case
    OrdLaw (OrdLawReflexive x) -> x <= x
    OrdLaw (OrdLawAntisymmetric x y) -> (x <= y && y <= x) --> (x == y)
    OrdLaw (OrdLawTransitive x y z) -> (x <= y && y <= z) --> (x <= z)

instance Variety Semigroup where
  type Requirements Semigroup = Eq
  data Signature Semigroup x
    = SemigroupAppend x x
    deriving (Show, Generic)
  newtype Operations Semigroup x = OperationsSemigroup x
  operations :: (Semigroup x) => Signature Semigroup x -> Operations Semigroup x
  operations = \case
    SemigroupAppend x y -> OperationsSemigroup (x <> y)
  data Laws Semigroup x
    = SemigroupAppendAssociative x x x
    deriving (Show, Generic)
  lawful ::
    (Semigroup x, Requirements Semigroup x) =>
    Laws Semigroup x -> Bool
  lawful = \case
    SemigroupAppendAssociative x y z -> x <> (y <> z) == (x <> y) <> z

instance Variety Monoid where
  type Requirements Monoid = Eq
  data Signature Monoid x
    = MonoidSemigroup (Signature Semigroup x)
    | MonoidMempty
    deriving (Show, Generic)
  newtype Operations Monoid x = OperationsMonoid x
  operations :: (Monoid x) => Signature Monoid x -> Operations Monoid x
  operations = \case
    MonoidSemigroup sig -> case operations sig of
      OperationsSemigroup op -> OperationsMonoid op
    MonoidMempty -> OperationsMonoid mempty
  data Laws Monoid x
    = MonoidSemigroupLaws (Laws Semigroup x)
    | MonoidMemptyLeft x
    | MonoidMemptyRight x
    deriving (Show, Generic)
  lawful ::
    (Monoid x, Requirements Monoid x) =>
    Laws Monoid x -> Bool
  lawful = \case
    MonoidSemigroupLaws laws -> lawful laws
    MonoidMemptyLeft x -> mempty <> x == x
    MonoidMemptyRight x -> x <> mempty == x

instance Variety Conjugate where
  type Requirements Conjugate = Eq
  data Signature Conjugate x
    = Conjugate x
    deriving (Show, Generic)
  newtype Operations Conjugate x = OperationsConjugate x
  operations :: (Conjugate x) => Signature Conjugate x -> Operations Conjugate x
  operations = \case
    Conjugate x -> OperationsConjugate (conjugate x)
  data Laws Conjugate x
    = ConjugateInvolution x
    deriving (Show, Generic)
  lawful ::
    (Conjugate x, Requirements Conjugate x) =>
    Laws Conjugate x -> Bool
  lawful = \case
    ConjugateInvolution x -> conjugate (conjugate x) == x

instance Variety Rack where
  type Requirements Rack = Eq
  data Signature Rack x
    = Lack x x
    | Rack x x
    deriving (Show, Generic)
  newtype Operations Rack x = OperationsRack x
  operations :: (Rack x) => Signature Rack x -> Operations Rack x
  operations = OperationsRack . \case
    Lack x y -> x <| y
    Rack x y -> x |> y
  data Laws Rack x
    = LackSelfDistributive x x x
    | RackSelfDistributive x x x
    | LackRackIdentity x x
    | RackLackIdentity x x
    deriving (Show, Generic)
  lawful ::
    (Rack x, Requirements Rack x) =>
    Laws Rack x -> Bool
  lawful = \case
    LackSelfDistributive x y z -> x <| (y <| z) == (x <| y) <| (x <| z)
    RackSelfDistributive x y z -> (x |> y) |> z == (x |> y) |> (x |> z)
    LackRackIdentity x y -> (x <| y) |> x == y
    RackLackIdentity x y -> x <| (y |> x) == y

instance Variety Quandle where
  type Requirements Quandle = Eq
  data Signature Quandle x
    = QuandleRack (Signature Rack x)
    deriving (Show, Generic)
  newtype Operations Quandle x = OperationsQuandle x
  operations :: (Quandle x) => Signature Quandle x -> Operations Quandle x
  operations = \case
    QuandleRack sig -> case operations sig of
      OperationsRack op -> OperationsQuandle op
  data Laws Quandle x
    = QuandleRackLaws (Laws Rack x)
    | QuandleIdempotentLack x
    | QuandleIdempotentRack x
    deriving (Show, Generic)
  lawful ::
    (Quandle x, Requirements Quandle x) =>
    Laws Quandle x -> Bool
  lawful = \case
    QuandleRackLaws laws -> lawful laws
    QuandleIdempotentLack x -> x <| x == x
    QuandleIdempotentRack x -> x |> x == x

instance Variety Additive where
  type Requirements Additive = Eq
  data Signature Additive x
    = AdditiveZero
    | AdditiveAdd x x
    deriving (Show, Generic)
  newtype Operations Additive x = OperationsAdditive x
  operations :: (Additive x) => Signature Additive x -> Operations Additive x
  operations = OperationsAdditive . \case
    AdditiveZero -> zero
    AdditiveAdd x y -> x + y
  data Laws Additive x
    = AdditiveZeroLeft x
    | AdditiveZeroRight x
    | AdditiveAssociative x x x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (Additive x, Requirements Additive x) =>
    Laws Additive x -> Bool
  lawful = \case
    AdditiveZeroLeft x -> zero @x + x == x
    AdditiveZeroRight x -> x + zero @x == x
    AdditiveAssociative x y z -> x + (y + z) == (x + y) + z

instance Variety AdditiveAbelian where
  type Requirements AdditiveAbelian = Eq
  data Signature AdditiveAbelian x
    = AdditiveAbelianAdditive (Signature Additive x)
    deriving (Show, Generic)
  newtype Operations AdditiveAbelian x = OperationsAdditiveAbelian x
  operations ::
    (AdditiveAbelian x) =>
    Signature AdditiveAbelian x ->
    Operations AdditiveAbelian x
  operations = \case
    AdditiveAbelianAdditive sig -> case operations sig of
      OperationsAdditive op -> OperationsAdditiveAbelian op
  data Laws AdditiveAbelian x
    = AdditiveAbelianAdditiveLaws (Laws Additive x)
    | AdditiveAbelianCommutative x x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (AdditiveAbelian x, Requirements Additive x) =>
    Laws AdditiveAbelian x -> Bool
  lawful = \case
    AdditiveAbelianAdditiveLaws laws -> lawful laws
    AdditiveAbelianCommutative x y -> x + y == y + x

instance Variety AdditiveGroup where
  type Requirements AdditiveGroup = Eq
  data Signature AdditiveGroup x
    = AdditiveGroupAdditive (Signature Additive x)
    | AdditiveGroupNegative x
    | AdditiveGroupSubtract x x
    deriving (Show, Generic)
  newtype Operations AdditiveGroup x = OperationsAdditiveGroup x
  operations ::
    (AdditiveGroup x) =>
    Signature AdditiveGroup x ->
    Operations AdditiveGroup x
  operations = \case
    AdditiveGroupAdditive sig -> case operations sig of
      OperationsAdditive op -> OperationsAdditiveGroup op
    AdditiveGroupNegative x -> OperationsAdditiveGroup (negative x)
    AdditiveGroupSubtract x y -> OperationsAdditiveGroup (x - y)
  data Laws AdditiveGroup x
    = AdditiveGroupAdditiveLaws (Laws Additive x)
    | AdditiveGroupNegativeZeroZero
    | AdditiveGroupNegativeAddSelfZero x
    | AdditiveGroupSubtractSelfZero x
    | AdditiveGroupSubtractZeroSelf x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (AdditiveGroup x, Requirements AdditiveGroup x) =>
    Laws AdditiveGroup x -> Bool
  lawful = \case
    AdditiveGroupAdditiveLaws laws -> lawful laws
    AdditiveGroupNegativeZeroZero -> negative zero == zero @x
    AdditiveGroupNegativeAddSelfZero x -> negative x + x == zero @x
    AdditiveGroupSubtractSelfZero x -> x - x == zero @x
    AdditiveGroupSubtractZeroSelf x -> x - zero @x == x

instance Variety Multiplicative where
  type Requirements Multiplicative = Eq
  data Signature Multiplicative x
    = MultiplicativeOne
    | MultiplicativeMultiply x x
    deriving (Show, Generic)
  newtype Operations Multiplicative x = OperationsMultiplicative x
  operations ::
    (Multiplicative x) =>
    Signature Multiplicative x ->
    Operations Multiplicative x
  operations = \case
    MultiplicativeOne -> OperationsMultiplicative one
    MultiplicativeMultiply x y -> OperationsMultiplicative (x * y)
  data Laws Multiplicative x
    = MultiplicativeOneLeft x
    | MultiplicativeOneRight x
    | MultiplicativeAssociative x x x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (Multiplicative x, Requirements Multiplicative x) =>
    Laws Multiplicative x -> Bool
  lawful = \case
    MultiplicativeOneLeft x -> one @x * x == x
    MultiplicativeOneRight x -> x * one @x == x
    MultiplicativeAssociative x y z -> x * (y * z) == (x * y) * z

instance Variety MultiplicativeAbelian where
  type Requirements MultiplicativeAbelian = Eq
  data Signature MultiplicativeAbelian x
    = MultiplicativeAbelianMultiplicative (Signature Multiplicative x)
    deriving (Show, Generic)
  newtype Operations MultiplicativeAbelian x = OperationsMultiplicativeAbelian x
  operations ::
    (MultiplicativeAbelian x) =>
    Signature MultiplicativeAbelian x ->
    Operations MultiplicativeAbelian x
  operations = \case
    MultiplicativeAbelianMultiplicative sig -> case operations sig of
      OperationsMultiplicative op -> OperationsMultiplicativeAbelian op
  data Laws MultiplicativeAbelian x
    = MultiplicativeAbelianMultiplicativeLaws (Laws Multiplicative x)
    | MultiplicativeAbelianCommutative x x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (MultiplicativeAbelian x, Requirements MultiplicativeAbelian x) =>
    Laws MultiplicativeAbelian x -> Bool
  lawful = \case
    MultiplicativeAbelianMultiplicativeLaws laws -> lawful laws
    MultiplicativeAbelianCommutative x y -> x * y == y * x

instance Variety MultiplicativeGroup where
  type Requirements MultiplicativeGroup = C2 Eq Additive
  data Signature MultiplicativeGroup x
    = MultiplicativeGroupMultiplicative (Signature Multiplicative x)
    | MultiplicativeGroupReciprocal x
    | MultiplicativeGroupDivide x x
    deriving (Show, Generic)
  newtype Operations MultiplicativeGroup x = OperationsMultiplicativeGroup x
  operations ::
    (MultiplicativeGroup x) =>
    Signature MultiplicativeGroup x ->
    Operations MultiplicativeGroup x
  operations = \case
    MultiplicativeGroupMultiplicative sig -> case operations sig of
      OperationsMultiplicative op -> OperationsMultiplicativeGroup op
    MultiplicativeGroupReciprocal x -> OperationsMultiplicativeGroup (reciprocal x)
    MultiplicativeGroupDivide x y -> OperationsMultiplicativeGroup (x / y)
  data Laws MultiplicativeGroup x
    = MultiplicativeGroupMultiplicativeLaws (Laws Multiplicative x)
    | MultiplicativeGroupSelfReciprocalOne x
    | MultiplicativeGroupDivideSelfOne x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (MultiplicativeGroup x, Requirements MultiplicativeGroup x) =>
    Laws MultiplicativeGroup x -> Bool
  lawful = \case
    MultiplicativeGroupMultiplicativeLaws laws -> lawful laws
    MultiplicativeGroupSelfReciprocalOne x ->
      (x /= zero @x) --> (x * reciprocal x == one @x)
    MultiplicativeGroupDivideSelfOne x ->
      (x /= zero @x) --> (x / x == one @x)

instance Variety Distributive where
  type Requirements Distributive = Eq
  data Signature Distributive x
    = DistributiveAdditive (Signature Additive x)
    | DistributiveMultiplicative (Signature Multiplicative x)
    deriving (Show, Generic)
  newtype Operations Distributive x = OperationsDistributive x
  operations ::
    (Distributive x) =>
    Signature Distributive x ->
    Operations Distributive x
  operations = \case
    DistributiveAdditive sig -> case operations sig of
      OperationsAdditive op -> OperationsDistributive op
    DistributiveMultiplicative sig -> case operations sig of
      OperationsMultiplicative op -> OperationsDistributive op
  data Laws Distributive x
    = DistributiveAdditiveLaws (Laws Additive x)
    | DistributiveMultiplicativeLaws (Laws Multiplicative x)
    | DistributiveLeft x x x
    | DistributiveRight x x x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (Distributive x, Requirements Distributive x) =>
    Laws Distributive x -> Bool
  lawful = \case
    DistributiveAdditiveLaws laws -> lawful laws
    DistributiveMultiplicativeLaws laws -> lawful laws
    DistributiveLeft a b c -> a * (b + c) == a * b + a * c
    DistributiveRight a b c -> (a + b) * c == (a * c) + (b * c)

instance Variety Semiring where
  type Requirements Semiring = Eq
  data Signature Semiring x
    = SemiringDistributive (Signature Distributive x)
    deriving (Show, Generic)
  newtype Operations Semiring x = OperationsSemiring x
  operations ::
    (Semiring x) =>
    Signature Semiring x ->
    Operations Semiring x
  operations = \case
    SemiringDistributive sig -> case operations sig of
      OperationsDistributive op -> OperationsSemiring op
  data Laws Semiring x
    = SemiringDistributiveLaws (Laws Distributive x)
    | SemiringZeroMulZero x
    | SemiringMulZeroZero x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (Semiring x, Requirements Semiring x) =>
    Laws Semiring x -> Bool
  lawful = \case
    SemiringDistributiveLaws laws -> lawful laws
    SemiringZeroMulZero x -> zero @x * x == zero @x
    SemiringMulZeroZero x -> x * zero @x == zero @x

instance Variety Ring where
  type Requirements Ring = Eq
  data Signature Ring x
    = RingSemiring (Signature Semiring x)
    | RingAdditiveAbelian (Signature AdditiveAbelian x)
    | RingAdditiveGroup (Signature AdditiveGroup x)
    deriving (Show, Generic)
  newtype Operations Ring x = OperationsRing x
  operations :: (Ring x) => Signature Ring x -> Operations Ring x
  operations = \case
    RingSemiring sig -> case operations sig of
      OperationsSemiring op -> OperationsRing op
    RingAdditiveAbelian sig -> case operations sig of
      OperationsAdditiveAbelian op -> OperationsRing op
    RingAdditiveGroup sig -> case operations sig of
      OperationsAdditiveGroup op -> OperationsRing op
  data Laws Ring x
    = RingSemiringLaws (Laws Semiring x)
    | RingAdditiveAbelianLaws (Laws AdditiveAbelian x)
    | RingAdditiveGroupLaws (Laws AdditiveGroup x)
    deriving (Show, Generic)
  lawful :: (Ring x, Requirements Ring x) => Laws Ring x -> Bool
  lawful = \case
    RingSemiringLaws laws -> lawful laws
    RingAdditiveAbelianLaws laws -> lawful laws
    RingAdditiveGroupLaws laws -> lawful laws

instance Variety Domain where
  type Requirements Domain = Eq
  data Signature Domain x
    = DomainRing (Signature Ring x)
    deriving (Show, Generic)
  newtype Operations Domain x = OperationsDomain x
  operations ::
    (Domain x) =>
    Signature Domain x ->
    Operations Domain x
  operations = \case
    DomainRing sig -> case operations sig of
      OperationsRing x -> OperationsDomain x
  data Laws Domain x
    = DomainRingLaws (Laws Ring x)
    | DomainZeroProduct x x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (Domain x, Requirements Domain x) =>
    Laws Domain x -> Bool
  lawful = \case
    DomainRingLaws laws -> lawful laws
    DomainZeroProduct x y -> (x * y == zero @x) --> (x == zero || y == zero)

instance Variety IntegralDomain where
  type Requirements IntegralDomain = Eq
  data Signature IntegralDomain x
    = IntegralDomainDomain (Signature Domain x)
    deriving (Show, Generic)
  newtype Operations IntegralDomain x = OperationsIntegralDomain x
  operations ::
    (IntegralDomain x) =>
    Signature IntegralDomain x ->
    Operations IntegralDomain x
  operations = \case
    IntegralDomainDomain sig -> case operations sig of
      OperationsDomain x -> OperationsIntegralDomain x
  data Laws IntegralDomain x
    = IntegralDomainDomainLaws (Laws Domain x)
    | IntegralDomainMultiplicativeAbelian (Laws MultiplicativeAbelian x)
    deriving (Show, Generic)
  lawful ::
    forall x.
    (IntegralDomain x, Requirements IntegralDomain x) =>
    Laws IntegralDomain x -> Bool
  lawful = \case
    IntegralDomainDomainLaws laws -> lawful laws
    IntegralDomainMultiplicativeAbelian laws -> lawful laws

instance Variety Field where
  type Requirements Field = Eq
  data Signature Field x
    = FieldIntegralDomain (Signature IntegralDomain x)
    | FieldMultiplicativeGroup (Signature MultiplicativeGroup x)
    deriving (Show, Generic)
  newtype Operations Field x = OperationsField x
  operations :: (Field x) => Signature Field x -> Operations Field x
  operations = \case
    FieldIntegralDomain sig -> case operations sig of
      OperationsIntegralDomain op -> OperationsField op
    FieldMultiplicativeGroup sig -> case operations sig of
      OperationsMultiplicativeGroup op -> OperationsField op
  data Laws Field x
    = FieldIntegralDomainLaws (Laws IntegralDomain x)
    | FieldMultiplicativeGroupLaws (Laws MultiplicativeGroup x)
    | FieldZeroNeqOne
    deriving (Show, Generic)
  lawful ::
    forall x.
    (Field x, Requirements Field x) =>
    Laws Field x -> Bool
  lawful = \case
    FieldIntegralDomainLaws laws -> lawful laws
    FieldMultiplicativeGroupLaws laws -> lawful laws
    FieldZeroNeqOne -> zero @x /= one

instance Variety Euclidean where
  type Requirements Euclidean = C2 Eq Additive
  data Signature Euclidean x
    = EuclideanQuotient x x
    | EuclideanRemainder x x
    deriving (Show, Generic)
  newtype Operations Euclidean x = OperationsEuclidean x
  operations :: (Euclidean x) => Signature Euclidean x -> Operations Euclidean x
  operations = \case
    EuclideanQuotient n d -> OperationsEuclidean (quotient n d)
    EuclideanRemainder n d -> OperationsEuclidean (remainder n d)
  data Laws Euclidean x
    = EuclideanRemainderDegree x x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (Euclidean x, Requirements Euclidean x) =>
    Laws Euclidean x -> Bool
  lawful = \case
    EuclideanRemainderDegree n d ->
      (d /= zero @x) --> (degree (remainder n d) < degree d)

instance Variety Fractional where
  type Requirements Fractional = C2 Eq Additive
  data Signature Fractional x
    = Proper x
    deriving (Show, Generic)
  data Operations Fractional x
    = OperationsFractional (Integral x) x
  operations :: (Fractional x) => Signature Fractional x -> Operations Fractional x
  operations = \case
    Proper x -> case proper x of (i, f) -> OperationsFractional i f
  data Laws Fractional x
    = FractionalSum x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (Fractional x, Requirements Fractional x) =>
    Laws Fractional x -> Bool
  lawful = \case
    FractionalSum x -> case proper x of
      (l, y) -> x == l +. y

instance Variety Meet where
  type Requirements Meet = C0
  data Signature Meet x
    = Meet x x
    deriving (Show, Generic)
  newtype Operations Meet x = OperationsMeet x
  operations ::
    (Meet x, Requirements Meet x) =>
    Signature Meet x ->
    Operations Meet x
  operations = \case
    Meet x y -> OperationsMeet (x /\ y)
  data Laws Meet x
    = MeetAssociative x x x
    | MeetCommutative x x
    | MeetIdempotent x
    | MeetLessThan x x
    deriving (Show, Generic)
  lawful ::
    (Meet x, Requirements Meet x) =>
    Laws Meet x -> Bool
  lawful = \case
    MeetAssociative x y z -> x /\ (y /\ z) == (x /\ y) /\ z
    MeetCommutative x y -> x /\ y == y /\ x
    MeetIdempotent x -> x /\ x == x
    MeetLessThan x y -> (x /\ y) <= x && (x /\ y) <= y

instance Variety Lowest where
  type Requirements Lowest = C0
  data Signature Lowest x
    = LowestMeet (Signature Meet x)
    | LowestLowest
    deriving (Show, Generic)
  newtype Operations Lowest x = OperationsLowest x
  operations ::
    (Lowest x, Requirements Lowest x) =>
    Signature Lowest x ->
    Operations Lowest x
  operations = \case
    LowestMeet sig -> case operations sig of
      OperationsMeet x -> OperationsLowest x
    LowestLowest -> OperationsLowest lowest
  data Laws Lowest x
    = LowestMeetLaws (Laws Meet x)
    | LowestLeast x
    deriving (Show, Generic)
  lawful ::
    (Lowest x, Requirements Lowest x) =>
    Laws Lowest x -> Bool
  lawful = \case
    LowestMeetLaws laws -> lawful laws
    LowestLeast x -> lowest <= x

instance Variety Join where
  type Requirements Join = C0
  data Signature Join x
    = Join x x
    deriving (Show, Generic)
  newtype Operations Join x = OperationsJoin x
  operations ::
    (Join x, Requirements Join x) =>
    Signature Join x ->
    Operations Join x
  operations = \case
    Join x y -> OperationsJoin (x \/ y)
  data Laws Join x
    = JoinAssociative x x x
    | JoinCommutative x x
    | JoinIdempotent x
    | JoinGreaterThan x x
    deriving (Show, Generic)
  lawful ::
    (Join x, Requirements Join x) =>
    Laws Join x -> Bool
  lawful = \case
    JoinAssociative x y z -> x \/ (y \/ z) == (x \/ y) \/ z
    JoinCommutative x y -> x \/ y == y \/ x
    JoinIdempotent x -> x \/ x == x
    JoinGreaterThan x y -> x <= (x \/ y) && y <= (x \/ y)

instance Variety Highest where
  type Requirements Highest = C0
  data Signature Highest x
    = HighestJoin (Signature Join x)
    | HighestHighest
    deriving (Show, Generic)
  newtype Operations Highest x = OperationsHighest x
  operations ::
    (Highest x, Requirements Highest x) =>
    Signature Highest x ->
    Operations Highest x
  operations = \case
    HighestJoin sig -> case operations sig of
      OperationsJoin op -> OperationsHighest op
    HighestHighest -> OperationsHighest highest
  data Laws Highest x
    = HighestJoinLaws (Laws Join x)
    | HighestGreatest x
    deriving (Show, Generic)
  lawful ::
    (Highest x, Requirements Highest x) =>
    Laws Highest x -> Bool
  lawful = \case
    HighestJoinLaws laws -> lawful laws
    HighestGreatest x -> x <= highest

instance Variety Lattice where
  type Requirements Lattice = C0
  data Signature Lattice x
    = LatticeMeet (Signature Meet x)
    | LatticeJoin (Signature Join x)
    deriving (Show, Generic)
  newtype Operations Lattice x = OperationsLattice x
  operations ::
    (Lattice x, Requirements Lattice x) =>
    Signature Lattice x ->
    Operations Lattice x
  operations = \case
    LatticeMeet sig -> case operations sig of
      OperationsMeet op -> OperationsLattice op
    LatticeJoin sig -> case operations sig of
      OperationsJoin op -> OperationsLattice op
  data Laws Lattice x
    = LatticeMeetLaws (Laws Meet x)
    | LatticeJoinLaws (Laws Join x)
    | LatticeAbsorptionMeet x x
    | LatticeAbsorptionJoin x x
    deriving (Show, Generic)
  lawful ::
    (Lattice x, Requirements Lattice x) =>
    Laws Lattice x -> Bool
  lawful = \case
    LatticeMeetLaws laws -> lawful laws
    LatticeJoinLaws laws -> lawful laws
    LatticeAbsorptionMeet x y -> x /\ (x \/ y) == x
    LatticeAbsorptionJoin x y -> x \/ (x /\ y) == x

instance Variety Extrema where
  type Requirements Extrema = C0
  data Signature Extrema x
    = ExtremaLowest (Signature Lowest x)
    | ExtremaHighest (Signature Highest x)
    deriving (Show, Generic)
  newtype Operations Extrema x = OperationsExtrema x
  operations ::
    (Extrema x, Requirements Extrema x) =>
    Signature Extrema x ->
    Operations Extrema x
  operations = \case
    ExtremaLowest sig -> case operations sig of
      OperationsLowest op -> OperationsExtrema op
    ExtremaHighest sig -> case operations sig of
      OperationsHighest op -> OperationsExtrema op
  data Laws Extrema x
    = ExtremaLowestLaws (Laws Lowest x)
    | ExtremaHighestLaws (Laws Highest x)
    | ExtremaMeetHighest x
    | ExtremaJoinLowest x
    deriving (Show, Generic)
  lawful ::
    (Extrema x, Requirements Extrema x) =>
    Laws Extrema x -> Bool
  lawful = \case
    ExtremaLowestLaws laws -> lawful laws
    ExtremaHighestLaws laws -> lawful laws
    ExtremaMeetHighest x -> x /\ highest == x
    ExtremaJoinLowest x -> x \/ lowest == x

instance Variety Heyting where
  type Requirements Heyting = C0
  data Signature Heyting x
    = HeytingExtrema (Signature Extrema x)
    | HeytingImplies x x
    | HeytingComplement x
    deriving (Show, Generic)
  newtype Operations Heyting x = OperationsHeyting x
  operations ::
    (Heyting x, Requirements Heyting x) =>
    Signature Heyting x ->
    Operations Heyting x
  operations = \case
    HeytingExtrema sig -> case operations sig of
      OperationsExtrema op -> OperationsHeyting op
    HeytingImplies x y -> OperationsHeyting (x --> y)
    HeytingComplement x -> OperationsHeyting (complement x)
  data Laws Heyting x
    = HeytingExtremaLaws (Laws Extrema x)
    | HeytingDistributeMeet x x x
    | HeytingDistributeJoin x x x
    | HeytingImpliesSelfHighest x
    | HeytingMeetImpliesMeet x x
    | HeytingMeetImpliesSelf x x
    | HeytingImpliesMeetMeetImplies x x x
    | HeytingComplementSelfMeetLowest x
    deriving (Show, Generic)
  lawful ::
    (Heyting x, Requirements Heyting x) =>
    Laws Heyting x -> Bool
  lawful = \case
    HeytingExtremaLaws laws -> lawful laws
    HeytingDistributeMeet x y z -> x /\ (y \/ z) == (x /\ y) \/ (x /\ z)
    HeytingDistributeJoin x y z -> x \/ (y /\ z) == (x \/ y) /\ (x \/ z)
    HeytingImpliesSelfHighest x -> x --> x == highest
    HeytingMeetImpliesMeet x y -> x /\ (x --> y) == x /\ y
    HeytingMeetImpliesSelf x y -> y /\ (x --> y) == y
    HeytingImpliesMeetMeetImplies x y z -> x --> (y /\ z) == (x --> y) /\ (x --> z)
    HeytingComplementSelfMeetLowest x -> x /\ complement x == lowest

instance Variety Boolean where
  type Requirements Boolean = C0
  data Signature Boolean x
    = BooleanHeyting (Signature Heyting x)
    deriving (Show, Generic)
  newtype Operations Boolean x = OperationsBoolean x
  operations ::
    (Boolean x, Requirements Boolean x) =>
    Signature Boolean x ->
    Operations Boolean x
  operations = \case
    BooleanHeyting sig -> case operations sig of
      OperationsHeyting op -> OperationsBoolean op
  data Laws Boolean x
    = BooleanHeytingLaws (Laws Heyting x)
    | BooleanComplementSelfJoinHighest x
    | BooleanDistributeJoin x x x
    | BooleanDistributeMeet x x x
    | BooleanImplicationComplement x x
    deriving (Show, Generic)
  lawful ::
    (Boolean x, Requirements Boolean x) =>
    Laws Boolean x -> Bool
  lawful = \case
    BooleanHeytingLaws laws -> lawful laws
    BooleanComplementSelfJoinHighest x -> x \/ complement x == highest
    BooleanDistributeMeet x y z -> x /\ (y \/ z) == (x /\ y) \/ (x /\ z)
    BooleanDistributeJoin x y z -> x \/ (y /\ z) == (x \/ y) /\ (x \/ z)
    BooleanImplicationComplement x y -> (x --> y) == (complement x \/ y)

instance Variety Median where
  type Requirements Median = Eq
  data Signature Median x
    = MedianBoolean (Signature Boolean x)
    | MedianMedian x x x
    deriving (Show, Generic)
  newtype Operations Median x = OperationsMedian x
  operations ::
    (Median x, Requirements Median x) =>
    Signature Median x ->
    Operations Median x
  operations = \case
    MedianBoolean sig -> case operations sig of
      OperationsBoolean op -> OperationsMedian op
    MedianMedian x y z -> OperationsMedian (median x y z)
  data Laws Median x
    = MedianMajority x x
    | MedianCyclic x x x
    | MedianAnticyclic x x x
    | MedianOfMedian x x x x
    deriving (Show, Generic)
  lawful :: (Median x, Requirements Median x) => Laws Median x -> Bool
  lawful = \case
    MedianMajority x y -> median x y y == y
    MedianCyclic x y z -> median x y z == median z x y
    MedianAnticyclic x y z -> median x y z == median x z y
    MedianOfMedian x y z w ->
      median (median x w y) w z == median x w (median y w z)

instance Variety Logarithmic where
  type Requirements Logarithmic = C2 Ord (C2 Additive Multiplicative)
  data Signature Logarithmic x
    = LogarithmicExp x
    | LogarithmicLog x
    | LogarithmicLogBase x x
    deriving (Show, Generic)
  newtype Operations Logarithmic x = OperationsLogarithmic x
  operations ::
    (Logarithmic x) =>
    Signature Logarithmic x ->
    Operations Logarithmic x
  operations = OperationsLogarithmic . \case
    LogarithmicExp x -> exp x
    LogarithmicLog x -> log x
    LogarithmicLogBase b x -> logBase b x
  data Laws Logarithmic x
    = LogarithmicExpLogIdentity x
    | LogarithmicLogExpIdentity x
    | LogarithmicExpSumProductExp x x
    | LogarithmicLogProductSumLog x x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (Logarithmic x, Requirements Logarithmic x) =>
    Laws Logarithmic x -> Bool
  lawful = \case
    LogarithmicExpLogIdentity x -> log (exp x) == x
    LogarithmicLogExpIdentity x -> x <= zero @x || exp (log x) == x
    LogarithmicExpSumProductExp x y -> exp @x (x + y) == exp x * exp y
    LogarithmicLogProductSumLog x y -> log @x (x * y) == log x + log y

instance Variety Trigonometric where
  type Requirements Trigonometric = C2 Eq Field
  data Signature Trigonometric x
    = TrigonometricSin x
    | TrigonometricCos x
    | TrigonometricTan x
    | TrigonometricArcsin x
    | TrigonometricArccos x
    | TrigonometricArctan x
    deriving (Show, Generic)
  newtype Operations Trigonometric x = OperationsTrigonometric x
  operations ::
    (Trigonometric x) =>
    Signature Trigonometric x ->
    Operations Trigonometric x
  operations = OperationsTrigonometric . \case
    TrigonometricSin x -> sin x
    TrigonometricCos x -> cos x
    TrigonometricTan x -> tan x
    TrigonometricArcsin x -> arcsin x
    TrigonometricArccos x -> arccos x
    TrigonometricArctan x -> arctan x
  data Laws Trigonometric x
    = TrigonometricPythagoras x
    | TrigonometricSinOdd x
    | TrigonometricCosEven x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (Trigonometric x, Requirements Trigonometric x) =>
    Laws Trigonometric x -> Bool
  lawful = \case
    TrigonometricPythagoras x ->
      sin x * sin x + cos x * cos x == one @x
    TrigonometricSinOdd x -> sin (negative x) == negative (sin x)
    TrigonometricCosEven x -> cos (negative x) == cos x

instance Variety Root where
  type Requirements Root = C2 Eq Field
  data Signature Root x
    = RootField (Signature Field x)
    | RootPower x Rational
    deriving (Show, Generic)
  newtype Operations Root x = OperationsRoot x
  operations :: (Root x) => Signature Root x -> Operations Root x
  operations = \case
    RootField sig -> case operations sig of
      OperationsField op -> OperationsRoot op
    RootPower x r -> OperationsRoot (x ^ r)
  data Laws Root x
    = RootFieldLaws (Laws Field x)
    | RootPowerZero x
    | RootPowerOne x
    | RootPowerReciprocal x Rational
    deriving (Show, Generic)
  lawful :: forall x. (Root x, Requirements Root x) => Laws Root x -> Bool
  lawful = \case
    RootFieldLaws laws -> lawful laws
    RootPowerZero x -> (x ^ (zero :: Rational)) == one @x
    RootPowerOne x -> (x ^ (one :: Rational)) == x
    RootPowerReciprocal x r@(Ratio n d) -> ((x ^ r :: x) ^ (reduce d n)) == x

instance Variety (Morphisms (->) (->)) where
  type Requirements (Morphisms (->) (->)) = Eq1
  data Signature (Morphisms (->) (->)) f
    = forall x y. MorphismsFunction (x -> y)
  data Operations (Morphisms (->) (->)) f
    = forall x y. OperationsMorphism (f x -> f y)
  operations ::
    (Morphisms (->) (->) f) =>
    Signature (Morphisms (->) (->)) f ->
    Operations (Morphisms (->) (->)) f
  operations = \case
    MorphismsFunction x_y -> OperationsMorphism (morphism x_y)
  data Laws (Morphisms (->) (->)) f
    = forall x. (Eq x) => MorphismIdentityIdentity (f x)
  lawful ::
    ( Morphisms (->) (->) f
    , Requirements (Morphisms (->) (->)) f
    ) =>
    Laws (Morphisms (->) (->)) f -> Bool
  lawful = \case
    MorphismIdentityIdentity fx -> fx == morphism (id :: x -> x) fx

instance Variety (Traversals (->) (->)) where
  type Requirements (Traversals (->) (->)) = Eq1
  data Signature (Traversals (->) (->)) f
    = forall x y g. (Applicative g) => TraversalsFunction (x -> g y)
  data Operations (Traversals (->) (->)) f
    = forall x y g. (Applicative g) => OperationsTraverse (f x -> g (f y))
  operations ::
    (Traversals (->) (->) f) =>
    Signature (Traversals (->) (->)) f ->
    Operations (Traversals (->) (->)) f
  operations = \case
    TraversalsFunction x_gy -> OperationsTraverse (traverse x_gy)
  data Laws (Traversals (->) (->)) f
    = forall x. (Eq x) => TraverseIdentityIdentity (f x)
    | forall x g. (Eq x, Eq1 g, Applicative g) => TraversePurePure (Proxy g, f x)
  lawful ::
    ( Traversals (->) (->) x
    , Requirements (Traversals (->) (->)) x
    ) =>
    Laws (Traversals (->) (->)) x -> Bool
  lawful = \case
    TraverseIdentityIdentity fx -> Identity fx == traverse Identity fx
    TraversePurePure (Proxy :: Proxy g, fx) -> pure @g fx == traverse (pure @g) fx

instance Variety (Traversals1 (->) (->)) where
  type Requirements (Traversals1 (->) (->)) = Eq1
  data Signature (Traversals1 (->) (->)) f
    = forall x y g. (Apply g) => Traversals1Function (x -> g y)
  data Operations (Traversals1 (->) (->)) f
    = forall x y g. (Apply g) => OperationsTraverse1 (f x -> g (f y))
  operations ::
    (Traversals1 (->) (->) f) =>
    Signature (Traversals1 (->) (->)) f ->
    Operations (Traversals1 (->) (->)) f
  operations = \case
    Traversals1Function x_gy -> OperationsTraverse1 (traverse1 x_gy)
  data Laws (Traversals1 (->) (->)) f
    = forall x. (Eq x) => Traverse1IdentityIdentity (f x)
    | forall x g. (Eq x, Eq1 g, Applicative g) => Traverse1PurePure (Proxy g, f x)
  lawful ::
    ( Traversals1 (->) (->) x
    , Requirements (Traversals1 (->) (->)) x
    ) =>
    Laws (Traversals1 (->) (->)) x -> Bool
  lawful = \case
    Traverse1IdentityIdentity fx -> Identity fx == traverse1 Identity fx
    Traverse1PurePure (Proxy :: Proxy g, fx) -> pure @g fx == traverse1 (pure @g) fx
