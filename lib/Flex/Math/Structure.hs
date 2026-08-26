{-# OPTIONS_GHC -Wno-duplicate-exports #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Flex.Math.Structure
  ( Structure (Requirements, Signature, Term, operations, Laws, lawful)
  , Signature (..)
  , Term (..)
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
import Data.Functor.Product (Product)
import Data.Kind (Constraint, Type)
import Data.Monoid (Monoid (mempty))
import Data.Ord (Ord (..))
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup ((<>)))
import GHC.Generics (Generic)
import GHC.Show (Show)

class Structure (var :: k -> Constraint) where
  data Signature var :: k -> Type
  data Term var :: k -> Type
  operations :: (var x) => Signature var x -> Term var x
  type Requirements var :: k -> Constraint
  data Laws var :: k -> Type
  lawful :: (var x, Requirements var x) => Laws var x -> Bool
class (Structure var, forall x y. (var x, var y) => var (x, y)) => Variety var
class (Structure var, forall f g. (var f, var g) => var (Product f g)) => Variety1 var

instance Structure Eq where
  data Signature Eq x deriving (Generic)
  data Term Eq x
  operations :: (Eq x) => Signature Eq x -> Term Eq x
  operations = \case {}
  type Requirements Eq = C0
  data Laws Eq x 
    = EqReflexive x
    | EqSymmetric x x
    | EqTransitive x x x
    deriving (Show, Generic)
  lawful :: (Eq x) => Laws Eq x -> Bool
  lawful = \case
    EqReflexive x -> x == x
    EqSymmetric x y -> (x == y) --> (y == x)
    EqTransitive x y z -> (x == y && y == z) --> (x == z)
instance Variety Eq

instance Structure Ord where
  data Signature Ord x deriving (Generic)
  data Term Ord x
  operations :: (Ord x) => Signature Ord x -> Term Ord x
  operations = \case {}
  type Requirements Ord = C0
  data Laws Ord x
    = OrdReflexive x
    | OrdAntisymmetric x x
    | OrdTransitive x x x
    deriving (Show, Generic)
  lawful :: (Ord x) => Laws Ord x -> Bool
  lawful = \case
    OrdReflexive x -> x <= x
    OrdAntisymmetric x y -> (x <= y && y <= x) --> (x == y)
    OrdTransitive x y z -> (x <= y && y <= z) --> (x <= z)
instance Variety Ord

instance Structure Semigroup where
  data Signature Semigroup x = SemigroupAppend x x
    deriving (Show, Generic)
  newtype Term Semigroup x = TermSemigroup x
  operations :: (Semigroup x) => Signature Semigroup x -> Term Semigroup x
  operations = \case
    SemigroupAppend x y -> TermSemigroup (x <> y)
  type Requirements Semigroup = Eq
  data Laws Semigroup x
    = SemigroupAppendAssociative x x x
    deriving (Show, Generic)
  lawful ::
    (Semigroup x, Requirements Semigroup x) =>
    Laws Semigroup x -> Bool
  lawful = \case
    SemigroupAppendAssociative x y z -> x <> (y <> z) == (x <> y) <> z
instance Variety Semigroup

instance Structure Monoid where
  data Signature Monoid x
    = MonoidSemigroup (Signature Semigroup x)
    | MonoidMempty
    deriving (Show, Generic)
  newtype Term Monoid x = TermMonoid x
  operations :: (Monoid x) => Signature Monoid x -> Term Monoid x
  operations = \case
    MonoidSemigroup sig -> case operations sig of
      TermSemigroup term -> TermMonoid term
    MonoidMempty -> TermMonoid mempty
  type Requirements Monoid = Eq
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
instance Variety Monoid

instance Structure Conjugate where
  newtype Signature Conjugate x = Conjugate x
    deriving (Show, Generic)
  newtype Term Conjugate x = TermConjugate x
  operations :: (Conjugate x) => Signature Conjugate x -> Term Conjugate x
  operations = \case
    Conjugate x -> TermConjugate (conjugate x)
  type Requirements Conjugate = Eq
  data Laws Conjugate x
    = ConjugateInvolution x
    deriving (Show, Generic)
  lawful ::
    (Conjugate x, Requirements Conjugate x) =>
    Laws Conjugate x -> Bool
  lawful = \case
    ConjugateInvolution x -> conjugate (conjugate x) == x

instance Structure Rack where
  data Signature Rack x
    = Lack x x
    | Rack x x
    deriving (Show, Generic)
  newtype Term Rack x = TermRack x
  operations :: (Rack x) => Signature Rack x -> Term Rack x
  operations = TermRack . \case
    Lack x y -> x <| y
    Rack x y -> x |> y
  type Requirements Rack = Eq
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
instance Variety Rack

instance Structure Quandle where
  newtype Signature Quandle x = QuandleRack (Signature Rack x)
    deriving (Show, Generic)
  newtype Term Quandle x = TermQuandle x
  operations :: (Quandle x) => Signature Quandle x -> Term Quandle x
  operations = \case
    QuandleRack sig -> case operations sig of
      TermRack term -> TermQuandle term
  type Requirements Quandle = Eq
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
instance Variety Quandle

instance Structure Additive where
  data Signature Additive x
    = AdditiveZero
    | AdditiveAdd x x
    deriving (Show, Generic)
  newtype Term Additive x = TermAdditive x
  operations :: (Additive x) => Signature Additive x -> Term Additive x
  operations = TermAdditive . \case
    AdditiveZero -> zero
    AdditiveAdd x y -> x + y
  type Requirements Additive = Eq
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
instance Variety Additive

instance Structure AdditiveAbelian where
  newtype Signature AdditiveAbelian x = AdditiveAbelianAdditive (Signature Additive x)
    deriving (Show, Generic)
  newtype Term AdditiveAbelian x = TermAdditiveAbelian x
  operations ::
    (AdditiveAbelian x) =>
    Signature AdditiveAbelian x ->
    Term AdditiveAbelian x
  operations = \case
    AdditiveAbelianAdditive sig -> case operations sig of
      TermAdditive term -> TermAdditiveAbelian term
  type Requirements AdditiveAbelian = Eq
  data Laws AdditiveAbelian x
    = AdditiveAbelianAdditiveLaws (Laws Additive x)
    | AdditiveAbelianCommutative x x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (AdditiveAbelian x, Requirements AdditiveAbelian x) =>
    Laws AdditiveAbelian x -> Bool
  lawful = \case
    AdditiveAbelianAdditiveLaws laws -> lawful laws
    AdditiveAbelianCommutative x y -> x + y == y + x
instance Variety AdditiveAbelian

instance Structure AdditiveGroup where
  data Signature AdditiveGroup x
    = AdditiveGroupAdditive (Signature Additive x)
    | AdditiveGroupNegative x
    | AdditiveGroupSubtract x x
    deriving (Show, Generic)
  newtype Term AdditiveGroup x = TermAdditiveGroup x
  operations ::
    (AdditiveGroup x) =>
    Signature AdditiveGroup x ->
    Term AdditiveGroup x
  operations = \case
    AdditiveGroupAdditive sig -> case operations sig of
      TermAdditive term -> TermAdditiveGroup term
    AdditiveGroupNegative x -> TermAdditiveGroup (negative x)
    AdditiveGroupSubtract x y -> TermAdditiveGroup (x - y)
  type Requirements AdditiveGroup = Eq
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
instance Variety AdditiveGroup

instance Structure AdditiveAbelianGroup where
  data Signature AdditiveAbelianGroup x
    = AdditiveAbelianGroupAdditiveAbelian (Signature AdditiveAbelian x)
    | AdditiveAbelianGroupAdditiveGroup (Signature AdditiveGroup x)
    deriving (Show, Generic)
  newtype Term AdditiveAbelianGroup x = TermAdditiveAbelianGroup x
  operations ::
    (AdditiveAbelianGroup x) =>
    Signature AdditiveAbelianGroup x ->
    Term AdditiveAbelianGroup x
  operations = \case
    AdditiveAbelianGroupAdditiveAbelian sig -> case operations sig of
      TermAdditiveAbelian term -> TermAdditiveAbelianGroup term
    AdditiveAbelianGroupAdditiveGroup sig -> case operations sig of
      TermAdditiveGroup term -> TermAdditiveAbelianGroup term
  data Laws AdditiveAbelianGroup x
    = LawsAdditiveAbelianGroupAdditiveAbelian (Laws AdditiveAbelian x)
    | LawsAdditiveAbelianGroupAdditiveGroup (Laws AdditiveGroup x)
    deriving (Show, Generic)
  type Requirements AdditiveAbelianGroup = Eq
  lawful ::
    forall x.
    (AdditiveAbelianGroup x, Requirements AdditiveAbelianGroup x) =>
    Laws AdditiveAbelianGroup x -> Bool
  lawful = \case
    LawsAdditiveAbelianGroupAdditiveAbelian laws -> lawful laws
    LawsAdditiveAbelianGroupAdditiveGroup laws -> lawful laws
instance Variety AdditiveAbelianGroup

instance Structure Multiplicative where
  data Signature Multiplicative x
    = MultiplicativeOne
    | MultiplicativeMultiply x x
    deriving (Show, Generic)
  newtype Term Multiplicative x = TermMultiplicative x
  operations ::
    (Multiplicative x) =>
    Signature Multiplicative x ->
    Term Multiplicative x
  operations = \case
    MultiplicativeOne -> TermMultiplicative one
    MultiplicativeMultiply x y -> TermMultiplicative (x * y)
  type Requirements Multiplicative = Eq
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
instance Variety Multiplicative

instance Structure MultiplicativeAbelian where
  newtype Signature MultiplicativeAbelian x
    = MultiplicativeAbelianMultiplicative (Signature Multiplicative x)
    deriving (Show, Generic)
  newtype Term MultiplicativeAbelian x = TermMultiplicativeAbelian x
  operations ::
    (MultiplicativeAbelian x) =>
    Signature MultiplicativeAbelian x ->
    Term MultiplicativeAbelian x
  operations = \case
    MultiplicativeAbelianMultiplicative sig -> case operations sig of
      TermMultiplicative term -> TermMultiplicativeAbelian term
  type Requirements MultiplicativeAbelian = Eq
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
instance Variety MultiplicativeAbelian

instance Structure MultiplicativeGroup where
  data Signature MultiplicativeGroup x
    = MultiplicativeGroupMultiplicative (Signature Multiplicative x)
    | MultiplicativeGroupReciprocal x
    | MultiplicativeGroupDivide x x
    deriving (Show, Generic)
  newtype Term MultiplicativeGroup x = TermMultiplicativeGroup x
  operations ::
    (MultiplicativeGroup x) =>
    Signature MultiplicativeGroup x ->
    Term MultiplicativeGroup x
  operations = \case
    MultiplicativeGroupMultiplicative sig -> case operations sig of
      TermMultiplicative term -> TermMultiplicativeGroup term
    MultiplicativeGroupReciprocal x -> TermMultiplicativeGroup (reciprocal x)
    MultiplicativeGroupDivide x y -> TermMultiplicativeGroup (x / y)
  type Requirements MultiplicativeGroup = C2 Eq Additive
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
instance Variety MultiplicativeGroup

instance Structure MultiplicativeAbelianGroup where
  data Signature MultiplicativeAbelianGroup x
    = MultiplicativeAbelianGroupMultiplicativeAbelian
        (Signature MultiplicativeAbelian x)
    | MultiplicativeAbelianGroupMultiplicativeGroup
        (Signature MultiplicativeGroup x)
    deriving (Show, Generic)
  newtype Term MultiplicativeAbelianGroup x = TermMultiplicativeAbelianGroup x
  operations ::
    (MultiplicativeAbelianGroup x) =>
    Signature MultiplicativeAbelianGroup x ->
    Term MultiplicativeAbelianGroup x
  operations = \case
    MultiplicativeAbelianGroupMultiplicativeAbelian sig -> case operations sig of
      TermMultiplicativeAbelian term -> TermMultiplicativeAbelianGroup term
    MultiplicativeAbelianGroupMultiplicativeGroup sig -> case operations sig of
      TermMultiplicativeGroup term -> TermMultiplicativeAbelianGroup term
  type Requirements MultiplicativeAbelianGroup = C2 Eq Additive
  data Laws MultiplicativeAbelianGroup x
    = LawsMultiplicativeAbelianGroupMultiplicativeAbelian (Laws MultiplicativeAbelian x)
    | LawsMultiplicativeAbelianGroupMultiplicativeGroup (Laws MultiplicativeGroup x)
    deriving (Show, Generic)
  lawful ::
    (MultiplicativeAbelianGroup x, Requirements MultiplicativeAbelianGroup x) =>
    Laws MultiplicativeAbelianGroup x -> Bool
  lawful = \case
    LawsMultiplicativeAbelianGroupMultiplicativeAbelian laws -> lawful laws
    LawsMultiplicativeAbelianGroupMultiplicativeGroup laws -> lawful laws

instance Structure Distributive where
  data Signature Distributive x
    = DistributiveAdditive (Signature Additive x)
    | DistributiveMultiplicative (Signature Multiplicative x)
    deriving (Show, Generic)
  newtype Term Distributive x = TermDistributive x
  operations ::
    (Distributive x) =>
    Signature Distributive x ->
    Term Distributive x
  operations = \case
    DistributiveAdditive sig -> case operations sig of
      TermAdditive term -> TermDistributive term
    DistributiveMultiplicative sig -> case operations sig of
      TermMultiplicative term -> TermDistributive term
  type Requirements Distributive = Eq
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
instance Variety Distributive

instance Structure Semiring where
  newtype Signature Semiring x = SemiringDistributive (Signature Distributive x)
    deriving (Show, Generic)
  newtype Term Semiring x = TermSemiring x
  operations ::
    (Semiring x) =>
    Signature Semiring x ->
    Term Semiring x
  operations = \case
    SemiringDistributive sig -> case operations sig of
      TermDistributive term -> TermSemiring term
  type Requirements Semiring = Eq
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
instance Variety Semiring

instance Structure Ring where
  data Signature Ring x
    = RingSemiring (Signature Semiring x)
    | RingAdditiveAbelian (Signature AdditiveAbelian x)
    | RingAdditiveGroup (Signature AdditiveGroup x)
    deriving (Show, Generic)
  newtype Term Ring x = TermRing x
  operations :: (Ring x) => Signature Ring x -> Term Ring x
  operations = \case
    RingSemiring sig -> case operations sig of
      TermSemiring term -> TermRing term
    RingAdditiveAbelian sig -> case operations sig of
      TermAdditiveAbelian term -> TermRing term
    RingAdditiveGroup sig -> case operations sig of
      TermAdditiveGroup term -> TermRing term
  type Requirements Ring = Eq
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
instance Variety Ring

instance Structure Domain where
  newtype Signature Domain x = DomainRing (Signature Ring x)
    deriving (Show, Generic)
  newtype Term Domain x = TermDomain x
  operations ::
    (Domain x) =>
    Signature Domain x ->
    Term Domain x
  operations = \case
    DomainRing sig -> case operations sig of
      TermRing x -> TermDomain x
  type Requirements Domain = Eq
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

instance Structure IntegralDomain where
  data Signature IntegralDomain x
    = IntegralDomainDomain (Signature Domain x)
    deriving (Show, Generic)
  newtype Term IntegralDomain x = TermIntegralDomain x
  operations ::
    (IntegralDomain x) =>
    Signature IntegralDomain x ->
    Term IntegralDomain x
  operations = \case
    IntegralDomainDomain sig -> case operations sig of
      TermDomain x -> TermIntegralDomain x
  type Requirements IntegralDomain = Eq
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

instance Structure Field where
  data Signature Field x
    = FieldIntegralDomain (Signature IntegralDomain x)
    | FieldMultiplicativeGroup (Signature MultiplicativeGroup x)
    deriving (Show, Generic)
  newtype Term Field x = TermField x
  operations :: (Field x) => Signature Field x -> Term Field x
  operations = \case
    FieldIntegralDomain sig -> case operations sig of
      TermIntegralDomain term -> TermField term
    FieldMultiplicativeGroup sig -> case operations sig of
      TermMultiplicativeGroup term -> TermField term
  type Requirements Field = Eq
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

instance Structure Euclidean where
  data Signature Euclidean x = Euclidean x x
    deriving (Show, Generic)
  newtype Term Euclidean x = TermEuclidean (x, x)
  operations :: (Euclidean x) => Signature Euclidean x -> Term Euclidean x
  operations = \case
    Euclidean n d -> TermEuclidean (euclidean n d)
  type Requirements Euclidean = C2 Eq Additive
  data Laws Euclidean x = EuclideanRemainderDegree x x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (Euclidean x, Requirements Euclidean x) =>
    Laws Euclidean x -> Bool
  lawful = \case
    EuclideanRemainderDegree n d ->
      (d /= zero @x) --> (degree (remainder n d) < degree d)

instance Structure Fractional where
  newtype Signature Fractional x = Proper x
    deriving (Show, Generic)
  newtype Term Fractional x = TermFractional (Integral x, x)
  operations :: (Fractional x) => Signature Fractional x -> Term Fractional x
  operations = \case
    Proper x -> TermFractional (proper x)
  type Requirements Fractional = C2 Eq Additive
  newtype Laws Fractional x = FractionalSum x
    deriving (Show, Generic)
  lawful ::
    forall x.
    (Fractional x, Requirements Fractional x) =>
    Laws Fractional x -> Bool
  lawful = \case
    FractionalSum x -> case proper x of (l, y) -> x == l +. y

instance Structure Meet where
  data Signature Meet x = Meet x x
    deriving (Show, Generic)
  newtype Term Meet x = TermMeet x
  operations ::
    (Meet x, Requirements Meet x) =>
    Signature Meet x ->
    Term Meet x
  operations = \case
    Meet x y -> TermMeet (x /\ y)
  type Requirements Meet = C0
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
instance Variety Meet

instance Structure Lowest where
  data Signature Lowest x
    = LowestMeet (Signature Meet x)
    | LowestLowest
    deriving (Show, Generic)
  newtype Term Lowest x = TermLowest x
  operations ::
    (Lowest x, Requirements Lowest x) =>
    Signature Lowest x ->
    Term Lowest x
  operations = \case
    LowestMeet sig -> case operations sig of
      TermMeet x -> TermLowest x
    LowestLowest -> TermLowest lowest
  type Requirements Lowest = C0
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
instance Variety Lowest

instance Structure Join where
  data Signature Join x = Join x x
    deriving (Show, Generic)
  newtype Term Join x = TermJoin x
  operations ::
    (Join x, Requirements Join x) =>
    Signature Join x ->
    Term Join x
  operations = \case
    Join x y -> TermJoin (x \/ y)
  type Requirements Join = C0
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
instance Variety Join

instance Structure Highest where
  data Signature Highest x
    = HighestJoin (Signature Join x)
    | HighestHighest
    deriving (Show, Generic)
  newtype Term Highest x = TermHighest x
  operations ::
    (Highest x, Requirements Highest x) =>
    Signature Highest x ->
    Term Highest x
  operations = \case
    HighestJoin sig -> case operations sig of
      TermJoin term -> TermHighest term
    HighestHighest -> TermHighest highest
  type Requirements Highest = C0
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
instance Variety Highest

instance Structure Lattice where
  data Signature Lattice x
    = LatticeMeet (Signature Meet x)
    | LatticeJoin (Signature Join x)
    deriving (Show, Generic)
  newtype Term Lattice x = TermLattice x
  operations ::
    (Lattice x, Requirements Lattice x) =>
    Signature Lattice x ->
    Term Lattice x
  operations = \case
    LatticeMeet sig -> case operations sig of
      TermMeet term -> TermLattice term
    LatticeJoin sig -> case operations sig of
      TermJoin term -> TermLattice term
  type Requirements Lattice = C0
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
instance Variety Lattice

instance Structure Extrema where
  data Signature Extrema x
    = ExtremaLowest (Signature Lowest x)
    | ExtremaHighest (Signature Highest x)
    deriving (Show, Generic)
  newtype Term Extrema x = TermExtrema x
  operations ::
    (Extrema x, Requirements Extrema x) =>
    Signature Extrema x ->
    Term Extrema x
  operations = \case
    ExtremaLowest sig -> case operations sig of
      TermLowest term -> TermExtrema term
    ExtremaHighest sig -> case operations sig of
      TermHighest term -> TermExtrema term
  type Requirements Extrema = C0
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
instance Variety Extrema

instance Structure Heyting where
  data Signature Heyting x
    = HeytingExtrema (Signature Extrema x)
    | HeytingImplies x x
    | HeytingComplement x
    deriving (Show, Generic)
  newtype Term Heyting x = TermHeyting x
  operations ::
    (Heyting x, Requirements Heyting x) =>
    Signature Heyting x ->
    Term Heyting x
  operations = \case
    HeytingExtrema sig -> case operations sig of
      TermExtrema term -> TermHeyting term
    HeytingImplies x y -> TermHeyting (x --> y)
    HeytingComplement x -> TermHeyting (complement x)
  type Requirements Heyting = C0
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
instance Variety Heyting

instance Structure Boolean where
  newtype Signature Boolean x = BooleanHeyting (Signature Heyting x)
    deriving (Show, Generic)
  newtype Term Boolean x = TermBoolean x
  operations ::
    (Boolean x, Requirements Boolean x) =>
    Signature Boolean x ->
    Term Boolean x
  operations = \case
    BooleanHeyting sig -> case operations sig of
      TermHeyting term -> TermBoolean term
  type Requirements Boolean = C0
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
instance Variety Boolean

instance Structure Median where
  data Signature Median x
    = MedianBoolean (Signature Boolean x)
    | MedianMedian x x x
    deriving (Show, Generic)
  newtype Term Median x = TermMedian x
  operations ::
    (Median x, Requirements Median x) =>
    Signature Median x ->
    Term Median x
  operations = \case
    MedianBoolean sig -> case operations sig of
      TermBoolean term -> TermMedian term
    MedianMedian x y z -> TermMedian (median x y z)
  type Requirements Median = Eq
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

instance Structure Logarithmic where
  data Signature Logarithmic x
    = LogarithmicExp x
    | LogarithmicLog x
    | LogarithmicLogBase x x
    deriving (Show, Generic)
  newtype Term Logarithmic x = TermLogarithmic x
  operations ::
    (Logarithmic x) =>
    Signature Logarithmic x ->
    Term Logarithmic x
  operations = TermLogarithmic . \case
    LogarithmicExp x -> exp x
    LogarithmicLog x -> log x
    LogarithmicLogBase b x -> logBase b x
  type Requirements Logarithmic = C2 Ord (C2 Additive Multiplicative)
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

instance Structure Trigonometric where
  data Signature Trigonometric x
    = TrigonometricSin x
    | TrigonometricCos x
    | TrigonometricTan x
    | TrigonometricArcsin x
    | TrigonometricArccos x
    | TrigonometricArctan x
    deriving (Show, Generic)
  newtype Term Trigonometric x = TermTrigonometric x
  operations ::
    (Trigonometric x) =>
    Signature Trigonometric x ->
    Term Trigonometric x
  operations = TermTrigonometric . \case
    TrigonometricSin x -> sin x
    TrigonometricCos x -> cos x
    TrigonometricTan x -> tan x
    TrigonometricArcsin x -> arcsin x
    TrigonometricArccos x -> arccos x
    TrigonometricArctan x -> arctan x
  type Requirements Trigonometric = C2 Eq Field
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

instance Structure Root where
  data Signature Root x
    = RootField (Signature Field x)
    | RootPower x Rational
    deriving (Show, Generic)
  newtype Term Root x = TermRoot x
  operations :: (Root x) => Signature Root x -> Term Root x
  operations = \case
    RootField sig -> case operations sig of
      TermField term -> TermRoot term
    RootPower x r -> TermRoot (x ^ r)
  type Requirements Root = C2 Eq Field
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

instance Structure (Morphisms (->) (->)) where
  data Signature (Morphisms (->) (->)) f
    = forall x y. MorphismsFunction (x -> y)
  data Term (Morphisms (->) (->)) f
    = forall x y. TermMorphism (f x -> f y)
  operations ::
    (Morphisms (->) (->) f) =>
    Signature (Morphisms (->) (->)) f ->
    Term (Morphisms (->) (->)) f
  operations = \case
    MorphismsFunction x_y -> TermMorphism (morphism x_y)
  type Requirements (Morphisms (->) (->)) = Eq1
  data Laws (Morphisms (->) (->)) f
    = forall x. (Eq x) => MorphismIdentityIdentity (f x)
  lawful ::
    ( Morphisms (->) (->) f
    , Requirements (Morphisms (->) (->)) f
    ) =>
    Laws (Morphisms (->) (->)) f -> Bool
  lawful = \case
    MorphismIdentityIdentity fx -> fx == morphism (id :: x -> x) fx
instance Variety1 (Morphisms (->) (->))

instance Structure (Traversals (->) (->)) where
  data Signature (Traversals (->) (->)) f
    = forall x y g. (Applicative g) => TraversalsFunction (x -> g y)
  data Term (Traversals (->) (->)) f
    = forall x y g. (Applicative g) => TermTraverse (f x -> g (f y))
  operations ::
    (Traversals (->) (->) f) =>
    Signature (Traversals (->) (->)) f ->
    Term (Traversals (->) (->)) f
  operations = \case
    TraversalsFunction x_gy -> TermTraverse (traverse x_gy)
  type Requirements (Traversals (->) (->)) = Eq1
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
instance Variety1 (Traversals (->) (->))

instance Structure (Traversals1 (->) (->)) where
  data Signature (Traversals1 (->) (->)) f
    = forall x y g. (Apply g) => Traversals1Function (x -> g y)
  data Term (Traversals1 (->) (->)) f
    = forall x y g. (Apply g) => TermTraverse1 (f x -> g (f y))
  operations ::
    (Traversals1 (->) (->) f) =>
    Signature (Traversals1 (->) (->)) f ->
    Term (Traversals1 (->) (->)) f
  operations = \case
    Traversals1Function x_gy -> TermTraverse1 (traverse1 x_gy)
  type Requirements (Traversals1 (->) (->)) = Eq1
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
instance Variety1 (Traversals1 (->) (->))
