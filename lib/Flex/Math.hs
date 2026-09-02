{-# LANGUAGE NamedDefaults #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Flex.Math
  ( -- * Conversion
    From (from)

    -- * Basic operations
  , Addition ((+.))
  , (+)
  , Subtraction ((-.))
  , (-)
  , Multiplication ((*.))
  , (*)
  , Division ((/.))
  , (/)
  , Power ((^))
  , Root ((√))
  , Conjugate (conjugate)
  , Absolute (absolute)
  , Sign (Positive, Unsigned, Negative)
  , Signed (sign)

    -- * Algebraic structures

    -- ** Numbers
  , Natural
  , Integer
  , Ratio (Ratio)
  , Rational
  , Ration
  , reduce
  , Additive (zero)
  , AdditiveAbelian
  , sum
  , AdditiveGroup (negative)
  , Multiplicative (one)
  , MultiplicativeAbelian
  , product
  , MultiplicativeGroup (reciprocal)
  , Distributive
  , Semiring
  , Ring
  , Domain
  , IntegralDomain
  , Field
  , Euclidean (degree, euclidean)
  , quotient
  , remainder
  , gcd
  , lcm
  , even
  , odd
  , Modulo
  , Fractional (Integral, proper)
  , fractional
  , truncate
  , ceiling
  , floor
  , round
  , Tropical (Tropical, Pole)

    -- *** Transcendental
  , Logarithmic (exp, log, logBase)
  , Trigonometric (pi, sin, cos, tan, arcsin, arccos, arctan)
  , Hyperbolic (sinh, cosh, tanh, arcsinh, arccosh, arctanh)

    -- *** Different number systems
  , Base (Base, getBase)
  , base
  , unbase
  , rebase
  , digits
  , undigits
  , Dual ((:&))
  , apply
  , primal
  , tangent
  , derivative
  , Perplex ((:!))
  , simple
  , perplex
  , Minkowski (Minkowski)
  , List1
  , eval

    -- ** Modules, vector spaces, algebras
  , Module (type Scalar)
  , Scalar (..)
  , Vector
  , Bilinear ((•))
  , Sesquilinear ((<•>))
  , InnerProduct
  , quadrance
  , Algebra
  , Unital
  , AssociativeAlgebra
  , CommutativeAlgebra
  , DivisionAlgebra
  , LieBracket

    -- *** Examples
  , Complex ((:+))
  , Quaternion (Quaternion)
  , V (V, unV)
  , dimensions
  , M (M, unM)
  , row
  , column
  , m22
  , m23
  , m24
  , m32
  , m33
  , m34
  , m42
  , m43
  , m44
  , Matrix (transpose)
  , adjoint
  , Square (trace, determinant)

    -- ** Lattices
  , Meet ((/\))
  , Join ((\/))
  , Lowest (lowest)
  , Highest (highest)
  , Lattice
  , Extrema
  , Heyting ((-->))
  , Boolean
  , Median (median)

    -- ** Other structures
  , Rack ((<|), (|>))
  , Quandle
  , Category (Objects, id, (.))
  , Groupoid (invert)

    -- * Extensions of number types
  , Projective (Projective, Infinity)
  , projective
  , Tropical (Tropical, Pole)
  , tropical
  , Suspension (South, Meridian, North)
  , suspension

    -- * Varieties
  , Structure (Requirements, Signature, Term, operations, Laws, lawful)
  , Signature (..)
  , Term (..)
  , Laws (..)

    -- * Other functions
  , loop
  , loopM
  , while

    -- ** Folds
  , length
  , all
  , iall
  , any
  , iany
  , and
  , or
  , sum
  , sumOn
  , isumOn
  , sumWhen
  , isumWhen
  , product
  , productOn
  , iproductOn
  , productWhen
  , iproductWhen
  , count
  , icount
  , maximum
  , minimum
  , maximumOn
  , minimumOn
  , maximumOf
  , minimumOf

    -- ** Bases
  , Basis (basis)
  ) where

import Flex.Math.Algebra
import Flex.Math.Base
import Flex.Math.Basis
import Flex.Math.Category
import Flex.Math.Dual
import Flex.Math.Foldable
import Flex.Math.Function
import Flex.Math.Lattice
import Flex.Math.LieBracket
import Flex.Math.Matrix
import Flex.Math.Minkowski
import Flex.Math.Module
import Flex.Math.Numbers
import Flex.Math.Perplex
import Flex.Math.Projective
import Flex.Math.Rack
import Flex.Math.Suspension
import Flex.Math.Structure

import Data.Eq (Eq)
import Data.Int (Int)
import Data.List1 (List1)
import GHC.Base (Double)
import GHC.Num (Integer, Num)
import GHC.Real qualified as Num
import Numeric.Natural (Natural)

default Num (Natural, Integer, Int, Rational, Double)
default Num.Integral (Natural, Integer, Int)
default Num.Fractional (Rational, Double)

eval :: forall v. (Eq v, Ring (Scalar v), Algebra v) => List1 (Scalar v) -> v -> v
eval ps x = foldr (\a p -> (a *. one @v) + x *. p) zero ps
