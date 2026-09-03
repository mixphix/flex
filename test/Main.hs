{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Applicative qualified as Control
import Control.Monad qualified as Control
import Data.Bool (Bool)
import Data.Eq
import Data.Functor qualified as Data
import Data.Int (Int)
import Data.Monoid
import Data.Ord
import Data.Semigroup (Semigroup)
import GHC.Err qualified as GHC
import GHC.Float (Double, Float)
import GHC.Num (Integer)
import Generic.Random (genericArbitrary, uniform)
import Numeric.Natural (Natural)
import System.IO (IO)
import Test.Hspec
import Test.QuickCheck

import Flex.Math
import Flex.Math.Matrix

instance (Ord x, Additive x, Arbitrary x) => Arbitrary (Ratio x) where
  arbitrary :: Gen (Ratio x)
  arbitrary = Control.liftM2 Ratio arbitrary (nonnegative arbitrary)
   where
    nonnegative g = sized \s -> do
      x <- if s == 0 then resize 1 g else g
      if x > zero
        then Control.pure x
        else nonnegative g

instance (Eq x, Arbitrary x) => Arbitrary (Laws Eq x) where
  arbitrary :: Gen (Laws Eq x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Ord x) where
  arbitrary :: Gen (Laws Ord x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Semigroup x) where
  arbitrary :: Gen (Laws Semigroup x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Monoid x) where
  arbitrary :: Gen (Laws Monoid x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Conjugate x) where
  arbitrary :: Gen (Laws Conjugate x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Rack x) where
  arbitrary :: Gen (Laws Rack x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Quandle x) where
  arbitrary :: Gen (Laws Quandle x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Additive x) where
  arbitrary :: Gen (Laws Additive x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws AdditiveAbelian x) where
  arbitrary :: Gen (Laws AdditiveAbelian x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws AdditiveGroup x) where
  arbitrary :: Gen (Laws AdditiveGroup x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Multiplicative x) where
  arbitrary :: Gen (Laws Multiplicative x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws MultiplicativeAbelian x) where
  arbitrary :: Gen (Laws MultiplicativeAbelian x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws MultiplicativeGroup x) where
  arbitrary :: Gen (Laws MultiplicativeGroup x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Distributive x) where
  arbitrary :: Gen (Laws Distributive x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Semiring x) where
  arbitrary :: Gen (Laws Semiring x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Ring x) where
  arbitrary :: Gen (Laws Ring x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Domain x) where
  arbitrary :: Gen (Laws Domain x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws IntegralDomain x) where
  arbitrary :: Gen (Laws IntegralDomain x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Field x) where
  arbitrary :: Gen (Laws Field x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Euclidean x) where
  arbitrary :: Gen (Laws Euclidean x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Fractional x) where
  arbitrary :: Gen (Laws Fractional x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Meet x) where
  arbitrary :: Gen (Laws Meet x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Lowest x) where
  arbitrary :: Gen (Laws Lowest x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Join x) where
  arbitrary :: Gen (Laws Join x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Highest x) where
  arbitrary :: Gen (Laws Highest x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Lattice x) where
  arbitrary :: Gen (Laws Lattice x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Extrema x) where
  arbitrary :: Gen (Laws Extrema x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Heyting x) where
  arbitrary :: Gen (Laws Heyting x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Boolean x) where
  arbitrary :: Gen (Laws Boolean x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Median x) where
  arbitrary :: Gen (Laws Median x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Logarithmic x) where
  arbitrary :: Gen (Laws Logarithmic x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Trigonometric x) where
  arbitrary :: Gen (Laws Trigonometric x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x) => Arbitrary (Laws Root x) where
  arbitrary :: Gen (Laws Root x)
  arbitrary = genericArbitrary uniform

instance
  (Eq (Scalar x), Eq x, Arbitrary x, Arbitrary (Scalar x)) =>
  Arbitrary (Laws Module x)
  where
  arbitrary :: Gen (Laws Module x)
  arbitrary = genericArbitrary uniform

instance
  (Eq (Scalar x), Eq x, Arbitrary x, Arbitrary (Scalar x)) =>
  Arbitrary (Laws Vector x)
  where
  arbitrary :: Gen (Laws Vector x)
  arbitrary = genericArbitrary uniform

instance
  (Eq (Scalar x), Eq x, Arbitrary x, Arbitrary (Scalar x)) =>
  Arbitrary (Laws Bilinear x)
  where
  arbitrary :: Gen (Laws Bilinear x)
  arbitrary = genericArbitrary uniform

instance
  (Eq (Scalar x), Eq x, Arbitrary x, Arbitrary (Scalar x)) =>
  Arbitrary (Laws Sesquilinear x)
  where
  arbitrary :: Gen (Laws Sesquilinear x)
  arbitrary = genericArbitrary uniform

instance
  (Eq (Scalar x), Eq x, Arbitrary x, Arbitrary (Scalar x)) =>
  Arbitrary (Laws InnerProduct x)
  where
  arbitrary :: Gen (Laws InnerProduct x)
  arbitrary = genericArbitrary uniform

instance (Arbitrary x) => Arbitrary (V 1 x) where
  arbitrary :: Gen (V 1 x)
  arbitrary = Data.fmap v1 arbitrary
instance (Arbitrary x) => Arbitrary (V 2 x) where
  arbitrary :: Gen (V 2 x)
  arbitrary = Control.liftM2 v2 arbitrary arbitrary
instance (Arbitrary x) => Arbitrary (V 3 x) where
  arbitrary :: Gen (V 3 x)
  arbitrary = Control.liftM3 v3 arbitrary arbitrary arbitrary
instance (Arbitrary x) => Arbitrary (V 4 x) where
  arbitrary :: Gen (V 4 x)
  arbitrary = Control.liftM3 v4 arbitrary arbitrary arbitrary Control.<*> arbitrary

instance (Arbitrary x) => Arbitrary (Quaternion x) where
  arbitrary :: Gen (Quaternion x)
  arbitrary = genericArbitrary uniform

instance (Arbitrary x) => Arbitrary (Minkowski x) where
  arbitrary :: Gen (Minkowski x)
  arbitrary = genericArbitrary uniform

instance
  (Eq (Scalar x), Eq x, Arbitrary x, Arbitrary (Scalar x)) =>
  Arbitrary (Laws Algebra x)
  where
  arbitrary :: Gen (Laws Algebra x)
  arbitrary = genericArbitrary uniform

instance
  (Eq (Scalar x), Eq x, Arbitrary x, Arbitrary (Scalar x)) =>
  Arbitrary (Laws Unital x)
  where
  arbitrary :: Gen (Laws Unital x)
  arbitrary = genericArbitrary uniform

instance
  (Eq (Scalar x), Eq x, Arbitrary x, Arbitrary (Scalar x)) =>
  Arbitrary (Laws AssociativeAlgebra x)
  where
  arbitrary :: Gen (Laws AssociativeAlgebra x)
  arbitrary = genericArbitrary uniform

instance
  (Eq (Scalar x), Eq x, Arbitrary x, Arbitrary (Scalar x)) =>
  Arbitrary (Laws CommutativeAlgebra x)
  where
  arbitrary :: Gen (Laws CommutativeAlgebra x)
  arbitrary = genericArbitrary uniform

instance
  (Eq (Scalar x), Eq x, Arbitrary x, Arbitrary (Scalar x)) =>
  Arbitrary (Laws DivisionAlgebra x)
  where
  arbitrary :: Gen (Laws DivisionAlgebra x)
  arbitrary = genericArbitrary uniform

instance (Eq x, Arbitrary x, Arbitrary (Scalar x)) => Arbitrary (Laws LieBracket x) where
  arbitrary :: Gen (Laws LieBracket x)
  arbitrary = genericArbitrary uniform

instance
  ( Eq x
  , Arbitrary x
  , Arbitrary (m x)
  , Eq (m x)
  , Eq (Scalar (m x))
  , Arbitrary (Scalar (m x))
  ) =>
  Arbitrary (Laws (Square m) x)
  where
  arbitrary :: Gen (Laws (Square m) x)
  arbitrary = genericArbitrary uniform

instance (Arbitrary x) => Arbitrary (M 2 2 x) where
  arbitrary :: Gen (M 2 2 x)
  arbitrary =
    Control.replicateM (2 * 2) arbitrary Control.>>= \case
      [a, b, c, d] -> Control.pure (m22 a b c d)
      _ -> GHC.error "arbitrary: replicateM incorrect"

instance (Arbitrary x) => Arbitrary (M 2 3 x) where
  arbitrary :: Gen (M 2 3 x)
  arbitrary =
    Control.replicateM (2 * 3) arbitrary Control.>>= \case
      [a, b, c, d, e, f] -> Control.pure (m23 a b c d e f)
      _ -> GHC.error "arbitrary: replicateM incorrect"

instance (Arbitrary x) => Arbitrary (M 2 4 x) where
  arbitrary :: Gen (M 2 4 x)
  arbitrary =
    Control.replicateM (2 * 4) arbitrary Control.>>= \case
      [a, b, c, d, e, f, g, h] -> Control.pure (m24 a b c d e f g h)
      _ -> GHC.error "arbitrary: replicateM incorrect"

instance (Arbitrary x) => Arbitrary (M 3 2 x) where
  arbitrary :: Gen (M 3 2 x)
  arbitrary =
    Control.replicateM (3 * 2) arbitrary Control.>>= \case
      [a, b, c, d, e, f] -> Control.pure (m32 a b c d e f)
      _ -> GHC.error "arbitrary: replicateM incorrect"

instance (Arbitrary x) => Arbitrary (M 3 3 x) where
  arbitrary :: Gen (M 3 3 x)
  arbitrary =
    Control.replicateM (3 * 3) arbitrary Control.>>= \case
      [a, b, c, d, e, f, g, h, i] -> Control.pure (m33 a b c d e f g h i)
      _ -> GHC.error "arbitrary: replicateM incorrect"

instance (Arbitrary x) => Arbitrary (M 3 4 x) where
  arbitrary :: Gen (M 3 4 x)
  arbitrary =
    Control.replicateM (3 * 4) arbitrary Control.>>= \case
      [a, b, c, d, e, f, g, h, i, j, k, l] -> Control.pure (m34 a b c d e f g h i j k l)
      _ -> GHC.error "arbitrary: replicateM incorrect"

instance (Arbitrary x) => Arbitrary (M 4 3 x) where
  arbitrary :: Gen (M 4 3 x)
  arbitrary =
    Control.replicateM (4 * 3) arbitrary Control.>>= \case
      [a, b, c, d, e, f, g, h, i, j, k, l] -> Control.pure (m43 a b c d e f g h i j k l)
      _ -> GHC.error "arbitrary: replicateM incorrect"

instance (Arbitrary x) => Arbitrary (M 4 4 x) where
  arbitrary :: Gen (M 4 4 x)
  arbitrary =
    Control.replicateM (4 * 4) arbitrary Control.>>= \case
      [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] -> Control.pure (m44 a b c d e f g h i j k l m n o p)
      _ -> GHC.error "arbitrary: replicateM incorrect"

instance (Arbitrary x) => Arbitrary (Suspension x) where
  arbitrary :: Gen (Suspension x)
  arbitrary = genericArbitrary uniform

instance (Arbitrary x) => Arbitrary (Projective x) where
  arbitrary :: Gen (Projective x)
  arbitrary = genericArbitrary uniform

deriving instance (Arbitrary x) => Arbitrary (Scalar (Complex x))
deriving instance (Arbitrary x) => Arbitrary (Scalar (Quaternion x))
deriving instance (Arbitrary x) => Arbitrary (Scalar (Minkowski x))
deriving instance (Arbitrary x) => Arbitrary (Scalar (V 1 x))
deriving instance (Arbitrary x) => Arbitrary (Scalar (V 2 x))
deriving instance (Arbitrary x) => Arbitrary (Scalar (V 3 x))
deriving instance (Arbitrary x) => Arbitrary (Scalar (V 4 x))
deriving instance (Arbitrary x) => Arbitrary (Scalar (M 2 2 x))
deriving instance (Arbitrary x) => Arbitrary (Scalar (M 2 3 x))
deriving instance (Arbitrary x) => Arbitrary (Scalar (M 2 4 x))
deriving instance (Arbitrary x) => Arbitrary (Scalar (M 3 2 x))
deriving instance (Arbitrary x) => Arbitrary (Scalar (M 3 3 x))
deriving instance (Arbitrary x) => Arbitrary (Scalar (M 3 4 x))
deriving instance (Arbitrary x) => Arbitrary (Scalar (M 4 2 x))
deriving instance (Arbitrary x) => Arbitrary (Scalar (M 4 3 x))
deriving instance (Arbitrary x) => Arbitrary (Scalar (M 4 4 x))

main :: IO ()
main = hspec do
  describe "Eq laws" do
    it "is lawful" do
      quickCheck @(Laws Eq Int -> Bool) lawful
      quickCheck @(Laws Eq Natural -> Bool) lawful
      quickCheck @(Laws Eq Integer -> Bool) lawful
      quickCheck @(Laws Eq Float -> Bool) lawful
      quickCheck @(Laws Eq Double -> Bool) lawful
      quickCheck @(Laws Eq (Complex Integer) -> Bool) lawful
      quickCheck @(Laws Eq (Complex Float) -> Bool) lawful
      quickCheck @(Laws Eq (Complex Double) -> Bool) lawful
      quickCheck @(Laws Eq (Projective Rational) -> Bool) lawful
      quickCheck @(Laws Eq (Suspension Rational) -> Bool) lawful
  describe "Ord laws" do
    it "is lawful" do
      quickCheck @(Laws Ord Int -> Bool) lawful
      quickCheck @(Laws Ord Natural -> Bool) lawful
      quickCheck @(Laws Ord Integer -> Bool) lawful
      quickCheck @(Laws Ord Float -> Bool) lawful
      quickCheck @(Laws Ord Double -> Bool) lawful
      quickCheck @(Laws Ord (Projective Rational) -> Bool) lawful
      quickCheck @(Laws Ord (Suspension Rational) -> Bool) lawful
  describe "Semigroup laws" do
    it "is lawful" do
      quickCheck @(Laws Semigroup (Sum Int) -> Bool) lawful
      quickCheck @(Laws Semigroup (Sum Natural) -> Bool) lawful
      quickCheck @(Laws Semigroup (Sum Integer) -> Bool) lawful
      quickCheck @(Laws Semigroup (Sum Rational) -> Bool) lawful
      quickCheck @(Laws Semigroup (Product Int) -> Bool) lawful
      quickCheck @(Laws Semigroup (Product Natural) -> Bool) lawful
      quickCheck @(Laws Semigroup (Product Integer) -> Bool) lawful
      quickCheck @(Laws Semigroup (Product Rational) -> Bool) lawful
  describe "Monoid laws" do
    it "is lawful" do
      quickCheck @(Laws Monoid (Sum Int) -> Bool) lawful
      quickCheck @(Laws Monoid (Sum Natural) -> Bool) lawful
      quickCheck @(Laws Monoid (Sum Integer) -> Bool) lawful
      quickCheck @(Laws Monoid (Sum Rational) -> Bool) lawful
      quickCheck @(Laws Monoid (Product Int) -> Bool) lawful
      quickCheck @(Laws Monoid (Product Natural) -> Bool) lawful
      quickCheck @(Laws Monoid (Product Integer) -> Bool) lawful
      quickCheck @(Laws Monoid (Product Rational) -> Bool) lawful
  describe "Conjugate laws" do
    it "is lawful" do
      quickCheck @(Laws Conjugate (Complex Integer) -> Bool) lawful
      quickCheck @(Laws Conjugate (Complex Rational) -> Bool) lawful
      quickCheck @(Laws Conjugate (Complex Float) -> Bool) lawful
      quickCheck @(Laws Conjugate (Complex Double) -> Bool) lawful
  describe "Additive laws" do
    it "is lawful" do
      quickCheck @(Laws Additive Int -> Bool) lawful
      quickCheck @(Laws Additive Natural -> Bool) lawful
      quickCheck @(Laws Additive Integer -> Bool) lawful
      quickCheck @(Laws Additive Rational -> Bool) lawful
      quickCheck @(Laws Additive (Complex Integer) -> Bool) lawful
      quickCheck @(Laws Additive (Complex Rational) -> Bool) lawful
  describe "AdditiveAbelian laws" do
    it "is lawful" do
      quickCheck @(Laws AdditiveAbelian Int -> Bool) lawful
      quickCheck @(Laws AdditiveAbelian Natural -> Bool) lawful
      quickCheck @(Laws AdditiveAbelian Integer -> Bool) lawful
      quickCheck @(Laws AdditiveAbelian Rational -> Bool) lawful
      quickCheck @(Laws AdditiveAbelian (Complex Integer) -> Bool) lawful
      quickCheck @(Laws AdditiveAbelian (Complex Rational) -> Bool) lawful
  describe "AdditiveGroup laws" do
    it "is lawful" do
      quickCheck @(Laws AdditiveGroup Int -> Bool) lawful
      quickCheck @(Laws AdditiveGroup Integer -> Bool) lawful
      quickCheck @(Laws AdditiveGroup Rational -> Bool) lawful
      quickCheck @(Laws AdditiveGroup (Complex Integer) -> Bool) lawful
      quickCheck @(Laws AdditiveGroup (Complex Rational) -> Bool) lawful
  describe "Multiplicative laws" do
    it "is lawful" do
      quickCheck @(Laws Multiplicative Int -> Bool) lawful
      quickCheck @(Laws Multiplicative Natural -> Bool) lawful
      quickCheck @(Laws Multiplicative Integer -> Bool) lawful
      quickCheck @(Laws Multiplicative Rational -> Bool) lawful
      quickCheck @(Laws Multiplicative (Complex Integer) -> Bool) lawful
      quickCheck @(Laws Multiplicative (Complex Rational) -> Bool) lawful
  describe "MultiplicativeAbelian laws" do
    it "is lawful" do
      quickCheck @(Laws MultiplicativeAbelian Int -> Bool) lawful
      quickCheck @(Laws MultiplicativeAbelian Natural -> Bool) lawful
      quickCheck @(Laws MultiplicativeAbelian Integer -> Bool) lawful
      quickCheck @(Laws MultiplicativeAbelian Rational -> Bool) lawful
      quickCheck @(Laws MultiplicativeAbelian (Complex Integer) -> Bool) lawful
      quickCheck @(Laws MultiplicativeAbelian (Complex Rational) -> Bool) lawful
  describe "MultiplicativeGroup laws" do
    it "is lawful" do
      quickCheck @(Laws MultiplicativeGroup Rational -> Bool) lawful
      quickCheck @(Laws MultiplicativeGroup (Complex Rational) -> Bool) lawful
  describe "Distributive laws" do
    it "is lawful" do
      quickCheck @(Laws Distributive Int -> Bool) lawful
      quickCheck @(Laws Distributive Natural -> Bool) lawful
      quickCheck @(Laws Distributive Integer -> Bool) lawful
      quickCheck @(Laws Distributive Rational -> Bool) lawful
      quickCheck @(Laws Distributive (Complex Integer) -> Bool) lawful
      quickCheck @(Laws Distributive (Complex Rational) -> Bool) lawful
  describe "Semiring laws" do
    it "is lawful" do
      quickCheck @(Laws Semiring Int -> Bool) lawful
      quickCheck @(Laws Semiring Natural -> Bool) lawful
      quickCheck @(Laws Semiring Integer -> Bool) lawful
      quickCheck @(Laws Semiring Rational -> Bool) lawful
      quickCheck @(Laws Semiring (Complex Integer) -> Bool) lawful
      quickCheck @(Laws Semiring (Complex Rational) -> Bool) lawful
  describe "Ring laws" do
    it "is lawful" do
      quickCheck @(Laws Ring Int -> Bool) lawful
      quickCheck @(Laws Ring Integer -> Bool) lawful
      quickCheck @(Laws Ring Rational -> Bool) lawful
      quickCheck @(Laws Ring (Complex Integer) -> Bool) lawful
      quickCheck @(Laws Ring (Complex Rational) -> Bool) lawful
  describe "Domain laws" do
    it "is lawful" do
      quickCheck @(Laws Domain Int -> Bool) lawful
      quickCheck @(Laws Domain Integer -> Bool) lawful
      quickCheck @(Laws Domain Rational -> Bool) lawful
      quickCheck @(Laws Domain (Complex Integer) -> Bool) lawful
      quickCheck @(Laws Domain (Complex Rational) -> Bool) lawful
  describe "IntegralDomain laws" do
    it "is lawful" do
      quickCheck @(Laws IntegralDomain Int -> Bool) lawful
      quickCheck @(Laws IntegralDomain Integer -> Bool) lawful
      quickCheck @(Laws IntegralDomain Rational -> Bool) lawful
      quickCheck @(Laws IntegralDomain (Complex Rational) -> Bool) lawful
  describe "Field laws" do
    it "is lawful" do
      quickCheck @(Laws Field Rational -> Bool) lawful
      quickCheck @(Laws Field (Complex Rational) -> Bool) lawful
  describe "Euclidean laws" do
    it "is lawful" do
      quickCheck @(Laws Euclidean Int -> Bool) lawful
      quickCheck @(Laws Euclidean Natural -> Bool) lawful
      quickCheck @(Laws Euclidean Integer -> Bool) lawful
  describe "Fractional laws" do
    it "is lawful" do
      quickCheck @(Laws Fractional Rational -> Bool) lawful
      quickCheck @(Laws Fractional Float -> Bool) lawful
      quickCheck @(Laws Fractional Double -> Bool) lawful
  describe "Meet laws" do
    it "is lawful" do
      quickCheck @(Laws Meet Bool -> Bool) lawful
  describe "Lowest laws" do
    it "is lawful" do
      quickCheck @(Laws Lowest Bool -> Bool) lawful
  describe "Join laws" do
    it "is lawful" do
      quickCheck @(Laws Join Bool -> Bool) lawful
  describe "Highest laws" do
    it "is lawful" do
      quickCheck @(Laws Highest Bool -> Bool) lawful
  describe "Lattice laws" do
    it "is lawful" do
      quickCheck @(Laws Lattice Bool -> Bool) lawful
  describe "Extrema laws" do
    it "is lawful" do
      quickCheck @(Laws Extrema Bool -> Bool) lawful
  describe "Heyting laws" do
    it "is lawful" do
      quickCheck @(Laws Heyting Bool -> Bool) lawful
  describe "Boolean laws" do
    it "is lawful" do
      quickCheck @(Laws Boolean Bool -> Bool) lawful
  describe "Median laws" do
    it "is lawful" do
      quickCheck @(Laws Median Bool -> Bool) lawful
  describe "Logarithmic laws" do
    it "is lawful" do
      quickCheck @(Laws Logarithmic Float -> Bool) lawful
      quickCheck @(Laws Logarithmic Double -> Bool) lawful
  describe "Trigonometric laws" do
    it "is lawful" do
      quickCheck @(Laws Trigonometric Float -> Bool) lawful
      quickCheck @(Laws Trigonometric Double -> Bool) lawful
  describe "Root laws" do
    it "is lawful" do
      quickCheck @(Laws Root Float -> Bool) lawful
      quickCheck @(Laws Root Double -> Bool) lawful
  describe "Module laws" do
    it "is lawful" do
      quickCheck @(Laws Module (V 1 Integer) -> Bool) lawful
      quickCheck @(Laws Module (V 2 Integer) -> Bool) lawful
      quickCheck @(Laws Module (V 3 Integer) -> Bool) lawful
      quickCheck @(Laws Module (V 4 Integer) -> Bool) lawful
      quickCheck @(Laws Module (M 2 2 Integer) -> Bool) lawful
      quickCheck @(Laws Module (M 2 3 Integer) -> Bool) lawful
      quickCheck @(Laws Module (M 3 2 Integer) -> Bool) lawful
      quickCheck @(Laws Module (M 3 3 Integer) -> Bool) lawful
      quickCheck @(Laws Module (M 3 4 Integer) -> Bool) lawful
      quickCheck @(Laws Module (M 4 3 Integer) -> Bool) lawful
      quickCheck @(Laws Module (M 4 4 Integer) -> Bool) lawful
      quickCheck @(Laws Module (Complex Integer) -> Bool) lawful
      quickCheck @(Laws Module (Quaternion Integer) -> Bool) lawful
      quickCheck @(Laws Module (V 1 Rational) -> Bool) lawful
      quickCheck @(Laws Module (V 2 Rational) -> Bool) lawful
      quickCheck @(Laws Module (V 3 Rational) -> Bool) lawful
      quickCheck @(Laws Module (V 4 Rational) -> Bool) lawful
      quickCheck @(Laws Module (M 2 2 Rational) -> Bool) lawful
      quickCheck @(Laws Module (M 2 3 Rational) -> Bool) lawful
      quickCheck @(Laws Module (M 3 2 Rational) -> Bool) lawful
      quickCheck @(Laws Module (M 3 3 Rational) -> Bool) lawful
      quickCheck @(Laws Module (M 3 4 Rational) -> Bool) lawful
      quickCheck @(Laws Module (M 4 3 Rational) -> Bool) lawful
      quickCheck @(Laws Module (M 4 4 Rational) -> Bool) lawful
      quickCheck @(Laws Module (Complex Rational) -> Bool) lawful
      quickCheck @(Laws Module (Quaternion Rational) -> Bool) lawful
      quickCheck @(Laws Module (Minkowski Rational) -> Bool) lawful
  describe "Vector laws" do
    it "is lawful" do
      quickCheck @(Laws Vector (V 1 Rational) -> Bool) lawful
      quickCheck @(Laws Vector (V 2 Rational) -> Bool) lawful
      quickCheck @(Laws Vector (V 3 Rational) -> Bool) lawful
      quickCheck @(Laws Vector (V 4 Rational) -> Bool) lawful
      quickCheck @(Laws Vector (M 2 2 Rational) -> Bool) lawful
      quickCheck @(Laws Vector (M 2 3 Rational) -> Bool) lawful
      quickCheck @(Laws Vector (M 3 2 Rational) -> Bool) lawful
      quickCheck @(Laws Vector (M 3 3 Rational) -> Bool) lawful
      quickCheck @(Laws Vector (M 3 4 Rational) -> Bool) lawful
      quickCheck @(Laws Vector (M 4 3 Rational) -> Bool) lawful
      quickCheck @(Laws Vector (M 4 4 Rational) -> Bool) lawful
      quickCheck @(Laws Vector (Complex Rational) -> Bool) lawful
      quickCheck @(Laws Vector (Quaternion Rational) -> Bool) lawful
  describe "Bilinear laws" do
    it "is lawful" do
      quickCheck @(Laws Bilinear (V 1 Rational) -> Bool) lawful
      quickCheck @(Laws Bilinear (V 2 Rational) -> Bool) lawful
      quickCheck @(Laws Bilinear (V 3 Rational) -> Bool) lawful
      quickCheck @(Laws Bilinear (V 4 Rational) -> Bool) lawful
      quickCheck @(Laws Bilinear (M 2 2 Rational) -> Bool) lawful
      quickCheck @(Laws Bilinear (M 3 3 Rational) -> Bool) lawful
      quickCheck @(Laws Bilinear (M 4 4 Rational) -> Bool) lawful
      quickCheck @(Laws Bilinear (Minkowski Rational) -> Bool) lawful
  describe "Sesquilinear laws" do
    it "is lawful" do
      quickCheck @(Laws Sesquilinear (V 1 (Complex Rational)) -> Bool) lawful
      quickCheck @(Laws Sesquilinear (V 2 (Complex Rational)) -> Bool) lawful
      quickCheck @(Laws Sesquilinear (V 3 (Complex Rational)) -> Bool) lawful
      quickCheck @(Laws Sesquilinear (V 4 (Complex Rational)) -> Bool) lawful
      quickCheck @(Laws Sesquilinear (M 2 2 (Complex Rational)) -> Bool) lawful
      quickCheck @(Laws Sesquilinear (M 3 3 (Complex Rational)) -> Bool) lawful
      quickCheck @(Laws Sesquilinear (M 4 4 (Complex Rational)) -> Bool) lawful
      quickCheck @(Laws Sesquilinear (Complex Rational) -> Bool) lawful
      quickCheck @(Laws Sesquilinear (Quaternion Rational) -> Bool) lawful
  describe "InnerProduct laws" do
    it "is lawful" do
      quickCheck @(Laws InnerProduct (V 1 Rational) -> Bool) lawful
      quickCheck @(Laws InnerProduct (V 2 Rational) -> Bool) lawful
      quickCheck @(Laws InnerProduct (V 3 Rational) -> Bool) lawful
      quickCheck @(Laws InnerProduct (V 4 Rational) -> Bool) lawful
      quickCheck @(Laws InnerProduct (M 2 2 Rational) -> Bool) lawful
      quickCheck @(Laws InnerProduct (M 3 3 Rational) -> Bool) lawful
      quickCheck @(Laws InnerProduct (M 4 4 Rational) -> Bool) lawful
      quickCheck @(Laws InnerProduct (V 1 (Complex Rational)) -> Bool) lawful
      quickCheck @(Laws InnerProduct (V 2 (Complex Rational)) -> Bool) lawful
      quickCheck @(Laws InnerProduct (V 3 (Complex Rational)) -> Bool) lawful
      quickCheck @(Laws InnerProduct (V 4 (Complex Rational)) -> Bool) lawful
      quickCheck @(Laws InnerProduct (M 2 2 (Complex Rational)) -> Bool) lawful
      quickCheck @(Laws InnerProduct (M 3 3 (Complex Rational)) -> Bool) lawful
      quickCheck @(Laws InnerProduct (M 4 4 (Complex Rational)) -> Bool) lawful
      quickCheck @(Laws InnerProduct (Complex Rational) -> Bool) lawful
      quickCheck @(Laws InnerProduct (Quaternion Rational) -> Bool) lawful
  describe "Algebra laws" do
    it "is lawful" do
      quickCheck @(Laws Algebra (M 2 2 Rational) -> Bool) lawful
      quickCheck @(Laws Algebra (M 3 3 Rational) -> Bool) lawful
      quickCheck @(Laws Algebra (M 4 4 Rational) -> Bool) lawful
      quickCheck @(Laws Algebra (Complex Rational) -> Bool) lawful
      quickCheck @(Laws Algebra (Quaternion Rational) -> Bool) lawful
  describe "Unital laws" do
    it "is lawful" do
      quickCheck @(Laws Unital (M 2 2 Rational) -> Bool) lawful
      quickCheck @(Laws Unital (M 3 3 Rational) -> Bool) lawful
      quickCheck @(Laws Unital (M 4 4 Rational) -> Bool) lawful
      quickCheck @(Laws Unital (Complex Rational) -> Bool) lawful
      quickCheck @(Laws Unital (Quaternion Rational) -> Bool) lawful
  describe "AssociativeAlgebra laws" do
    it "is lawful" do
      quickCheck @(Laws AssociativeAlgebra (M 2 2 Rational) -> Bool) lawful
      quickCheck @(Laws AssociativeAlgebra (M 3 3 Rational) -> Bool) lawful
      quickCheck @(Laws AssociativeAlgebra (M 4 4 Rational) -> Bool) lawful
      quickCheck @(Laws AssociativeAlgebra (Complex Rational) -> Bool) lawful
      quickCheck @(Laws AssociativeAlgebra (Quaternion Rational) -> Bool) lawful
  describe "CommutativeAlgebra laws" do
    it "is lawful" do
      quickCheck @(Laws CommutativeAlgebra (Complex Rational) -> Bool) lawful
  describe "DivisionAlgebra laws" do
    it "is lawful" do
      quickCheck @(Laws DivisionAlgebra (Complex Rational) -> Bool) lawful
      quickCheck @(Laws DivisionAlgebra (Quaternion Rational) -> Bool) lawful
  describe "LieBracket laws" do
    it "is lawful" do
      quickCheck @(Laws LieBracket (V 3 Rational) -> Bool) lawful
      quickCheck @(Laws LieBracket (M 2 2 Rational) -> Bool) lawful
      quickCheck @(Laws LieBracket (M 3 3 Rational) -> Bool) lawful
      quickCheck @(Laws LieBracket (M 4 4 Rational) -> Bool) lawful
  describe "Square laws" do
    it "is lawful" do
      quickCheck @(Laws (Square (M 2 2)) Rational -> Bool) lawful
      quickCheck @(Laws (Square (M 3 3)) Rational -> Bool) lawful
      quickCheck @(Laws (Square (M 4 4)) Rational -> Bool) lawful
