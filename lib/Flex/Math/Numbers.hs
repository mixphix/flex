{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

{- HLINT ignore "Redundant from" -}

module Flex.Math.Numbers
  ( -- * Conversion
    From (from)

    -- * Absolute, Signed, Conjugate
  , Absolute (absolute)
  , Sign (Negative, Unsigned, Positive)
  , Signed (sign)
  , Conjugate (conjugate)

    -- * Addition and Subtraction
  , Addition ((+.))
  , (+)
  , Subtraction ((-.))
  , (-)
  , poly

    -- * Multiplication and Division
  , Multiplication ((*.))
  , (*)
  , Division ((/.))
  , (/)

    -- * Power and Root
  , Power ((^))
  , Root ((√))

    -- * Algebraic structures
  , Additive (zero)
  , AdditiveAbelian
  , AdditiveGroup (negative)
  , AdditiveAbelianGroup
  , Multiplicative (one)
  , MultiplicativeAbelian
  , MultiplicativeGroup (reciprocal)
  , MultiplicativeAbelianGroup
  , Distributive
  , Semiring
  , Ring
  , Domain
  , IntegralDomain
  , Field

    -- * Euclidean domains and Ratio
  , Euclidean (euclidean, degree)
  , quotient
  , remainder
  , even
  , odd
  , gcd
  , lcm
  , evenOdd
  , Ratio (Ratio)
  , Rational
  , Ration
  , fromDataRational
  , toDataRational
  , reduce

    -- * Modulo
  , Modulo

    -- * Fractional
  , Fractional (Integral, proper)
  , fractional
  , truncate
  , ceiling
  , floor
  , round

    -- * Logarithmic
  , Logarithmic (exp, log, logBase)

    -- * Trigonometric and Hyperbolic
  , Trigonometric (pi, sin, cos, tan, arcsin, arccos, arctan)
  , Hyperbolic (sinh, cosh, tanh, arcsinh, arccosh, arctanh)
  ) where

import Flex.Math.Category
import Flex.Math.Function
import Flex.Math.Projective
import Flex.Math.Suspension

import Data.Bool (Bool (..), not, otherwise)
import Data.Bounded (Bounded)
import Data.Complex (Complex ((:+)))
import Data.Either
import Data.Enum (Enum (..))
import Data.Eq (Eq (..))
import Data.Finite (Finite, getFinite, modulo)
import Data.Fixed
import Data.Function (flip, on)
import Data.Int
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List1 hiding ((++))
import Data.Maybe
import Data.Ord (Ord (..), Ordering (..))
import Data.Proxy (Proxy (..))
import Data.Ratio qualified as Data
import Data.Semigroup
import Data.Time
import Data.Tuple (fst, snd)
import Data.Word
import GHC.Base
  ( Double (D#)
  , Float (F#)
  , and#
  , decodeFloat_Int#
  , eqWord#
  , int2Word#
  , isTrue#
  , negateInt#
  , (>=#)
  )
import GHC.Exception qualified as GHC
import GHC.Float qualified as Num
import GHC.Float.ConversionUtils (elimZerosInt#, elimZerosInteger)
import GHC.Generics (Generic)
import GHC.Num
  ( Integer (IS)
  , integerDecodeDouble#
  , integerShiftL#
  , integerToWord#
  )
import GHC.Num qualified as Num
import GHC.Real qualified as Num
import GHC.TypeNats (KnownNat, Nat, natVal)
import Numeric.Natural (Natural)
import Text.Read (Read)
import Text.Show (Show)

-- Ratio

data Ratio x = Ratio !x !x deriving (Show, Generic)

type Rational = Ratio Integer
type Ration = Ratio Natural

fromDataRational :: Data.Rational -> Rational
fromDataRational (n Num.:% d) = Ratio n d

toDataRational :: Rational -> Data.Rational
toDataRational (Ratio n d) = n Num.:% d

{-# SPECIALIZE reduce :: Integer -> Integer -> Rational #-}
{-# SPECIALIZE reduce :: Natural -> Natural -> Ration #-}
reduce ::
  (Eq x, From Integer x, Signed x, Absolute x x, Euclidean x) => x -> x -> Ratio x
reduce n d
  | d == zero = Num.ratioZeroDenominatorError
  | otherwise =
      let d' = absolute d
          p = gcd (absolute n) d'
       in Ratio ((n * signum d) `quotient` p) (d' `quotient` p)

instance (Eq x, Multiplication x x x) => Eq (Ratio x) where
  (==) :: Ratio x -> Ratio x -> Bool
  Ratio n0 d0 == Ratio n1 d1 = n0 * d1 == n1 * d0
instance (Ord x, Multiplication x x x) => Ord (Ratio x) where
  compare :: Ratio x -> Ratio x -> Ordering
  Ratio n0 d0 `compare` Ratio n1 d1 = (n0 * d1) `compare` (n1 * d0)
instance Absolute Rational Rational where
  absolute :: Rational -> Rational
  absolute x
    | x < zero = negative x
    | otherwise = x
instance Absolute Rational Ration where
  absolute :: Rational -> Ration
  absolute (Ratio n d) = reduce (absolute n) (absolute d)
instance Signed Rational where
  sign :: Rational -> Sign
  sign (Ratio n _) = case compare n zero of
    LT -> Negative
    EQ -> Unsigned
    GT -> Positive
instance Num.Num Rational where
  (+) :: Rational -> Rational -> Rational
  (+) = (+)
  (*) :: Rational -> Rational -> Rational
  (*) = (*)
  negate :: Rational -> Rational
  negate = negative
  abs :: Rational -> Rational
  abs = absolute
  signum :: Rational -> Rational
  signum = flip (.) sign \case
    Negative -> negative one
    Unsigned -> zero
    Positive -> one
  fromInteger :: Integer -> Rational
  fromInteger = from
instance Num.Fractional Rational where
  fromRational :: Num.Rational -> Rational
  fromRational = fromDataRational
  (/) :: Rational -> Rational -> Rational
  (/) = (/)

-- Conversion

class From x y where
  from :: x -> y
instance From x x where
  from :: x -> x
  from = id

instance From Int8 Int where
  from :: Int8 -> Int
  from = Num.fromIntegral
instance From Int8 Integer where
  from :: Int8 -> Integer
  from = Num.fromIntegral
instance From Int8 Rational where
  from :: Int8 -> Rational
  from i = Ratio (from i) one
instance From Natural Int8 where
  from :: Natural -> Int8
  from = Num.fromIntegral
instance From Int Int8 where
  from :: Int -> Int8
  from = Num.fromIntegral
instance From Integer Int8 where
  from :: Integer -> Int8
  from = Num.fromIntegral

instance From Word8 Natural where
  from :: Word8 -> Natural
  from = Num.fromIntegral
instance From Word8 Int where
  from :: Word8 -> Int
  from = Num.fromIntegral
instance From Word8 Integer where
  from :: Word8 -> Integer
  from = Num.fromIntegral
instance From Word8 Rational where
  from :: Word8 -> Rational
  from i = Ratio (from i) one
instance From Natural Word8 where
  from :: Natural -> Word8
  from = Num.fromIntegral
instance From Int Word8 where
  from :: Int -> Word8
  from = Num.fromIntegral
instance From Integer Word8 where
  from :: Integer -> Word8
  from = Num.fromIntegral

instance From Int16 Int where
  from :: Int16 -> Int
  from = Num.fromIntegral
instance From Int16 Integer where
  from :: Int16 -> Integer
  from = Num.fromIntegral
instance From Int16 Rational where
  from :: Int16 -> Rational
  from i = Ratio (from i) one
instance From Natural Int16 where
  from :: Natural -> Int16
  from = Num.fromIntegral
instance From Int Int16 where
  from :: Int -> Int16
  from = Num.fromIntegral
instance From Integer Int16 where
  from :: Integer -> Int16
  from = Num.fromIntegral

instance From Word16 Natural where
  from :: Word16 -> Natural
  from = Num.fromIntegral
instance From Word16 Int where
  from :: Word16 -> Int
  from = Num.fromIntegral
instance From Word16 Integer where
  from :: Word16 -> Integer
  from = Num.fromIntegral
instance From Word16 Rational where
  from :: Word16 -> Rational
  from i = Ratio (from i) one
instance From Natural Word16 where
  from :: Natural -> Word16
  from = Num.fromIntegral
instance From Int Word16 where
  from :: Int -> Word16
  from = Num.fromIntegral
instance From Integer Word16 where
  from :: Integer -> Word16
  from = Num.fromIntegral

instance From Int32 Int where
  from :: Int32 -> Int
  from = Num.fromIntegral
instance From Int32 Integer where
  from :: Int32 -> Integer
  from = Num.fromIntegral
instance From Int32 Rational where
  from :: Int32 -> Rational
  from i = Ratio (from i) one
instance From Natural Int32 where
  from :: Natural -> Int32
  from = Num.fromIntegral
instance From Int Int32 where
  from :: Int -> Int32
  from = Num.fromIntegral
instance From Integer Int32 where
  from :: Integer -> Int32
  from = Num.fromIntegral

instance From Word32 Natural where
  from :: Word32 -> Natural
  from = Num.fromIntegral
instance From Word32 Int where
  from :: Word32 -> Int
  from = Num.fromIntegral
instance From Word32 Integer where
  from :: Word32 -> Integer
  from = Num.fromIntegral
instance From Word32 Rational where
  from :: Word32 -> Rational
  from i = Ratio (from i) one
instance From Natural Word32 where
  from :: Natural -> Word32
  from = Num.fromIntegral
instance From Int Word32 where
  from :: Int -> Word32
  from = Num.fromIntegral
instance From Integer Word32 where
  from :: Integer -> Word32
  from = Num.fromIntegral

instance From Int64 Int where
  from :: Int64 -> Int
  from = Num.fromIntegral
instance From Int64 Integer where
  from :: Int64 -> Integer
  from = Num.fromIntegral
instance From Int64 Rational where
  from :: Int64 -> Rational
  from i = Ratio (from i) one
instance From Natural Int64 where
  from :: Natural -> Int64
  from = Num.fromIntegral
instance From Int Int64 where
  from :: Int -> Int64
  from = Num.fromIntegral
instance From Integer Int64 where
  from :: Integer -> Int64
  from = Num.fromIntegral

instance From Word64 Natural where
  from :: Word64 -> Natural
  from = Num.fromIntegral
instance From Word64 Int where
  from :: Word64 -> Int
  from = Num.fromIntegral
instance From Word64 Integer where
  from :: Word64 -> Integer
  from = Num.fromIntegral
instance From Word64 Rational where
  from :: Word64 -> Rational
  from i = Ratio (from i) one
instance From Natural Word64 where
  from :: Natural -> Word64
  from = Num.fromIntegral
instance From Int Word64 where
  from :: Int -> Word64
  from = Num.fromIntegral
instance From Integer Word64 where
  from :: Integer -> Word64
  from = Num.fromIntegral

instance From Int Integer where
  from :: Int -> Integer
  from = Num.fromIntegral
instance From Int Rational where
  from :: Int -> Rational
  from i = Ratio (from i) one
instance From Natural Int where
  from :: Natural -> Int
  from = Num.fromIntegral
instance From Integer Int where
  from :: Integer -> Int
  from = Num.fromIntegral

instance From Word Natural where
  from :: Word -> Natural
  from = Num.fromIntegral
instance From Word Int where
  from :: Word -> Int
  from = Num.fromIntegral
instance From Word Integer where
  from :: Word -> Integer
  from = Num.fromIntegral
instance From Word Rational where
  from :: Word -> Rational
  from i = Ratio (from i) one
instance From Natural Word where
  from :: Natural -> Word
  from = Num.fromIntegral
instance From Int Word where
  from :: Int -> Word
  from = Num.fromIntegral
instance From Integer Word where
  from :: Integer -> Word
  from = Num.fromIntegral

instance From Natural Integer where
  from :: Natural -> Integer
  from = Num.fromIntegral
instance From Natural Ration where
  from :: Natural -> Ration
  from n = Ratio n one
instance From Natural Rational where
  from :: Natural -> Rational
  from n = Ratio (from n) one

instance From Integer Natural where
  from :: Integer -> Natural
  from = Num.fromIntegral
instance From Int Natural where
  from :: Int -> Natural
  from = Num.fromIntegral

instance From Integer Rational where
  from :: Integer -> Rational
  from i = Ratio i one
instance From Ration Rational where
  from :: Ration -> Rational
  from (Ratio n d) = reduce (from n) (from d)

instance From Float Rational where
  from :: Float -> Rational
  from (F# x#) = case decodeFloat_Int# x# of
    (# m#, e# #)
      | isTrue# (e# >=# 0#) ->
          Ratio (IS m# `integerShiftL#` int2Word# e#) 1
      | isTrue# ((int2Word# m# `and#` 1##) `eqWord#` 0##) ->
          case elimZerosInt# m# (negateInt# e#) of
            (# n, d# #) -> Ratio n (integerShiftL# 1 (int2Word# d#))
      | otherwise ->
          Ratio (IS m#) (integerShiftL# 1 (int2Word# (negateInt# e#)))
instance From Natural Float where
  from :: Natural -> Float
  from = Num.fromIntegral
instance From Int Float where
  from :: Int -> Float
  from = Num.fromIntegral
instance From Integer Float where
  from :: Integer -> Float
  from = Num.fromIntegral
instance From Rational Float where
  from :: Rational -> Float
  {-# NOINLINE [0] from #-}
  from (Ratio n 0)
    | n == 0 = 0 Num./ 0
    | n < 0 = (-1) Num./ 0
    | otherwise = 1 Num./ 0
  from (Ratio n d)
    | n == 0 = Num.encodeFloat 0 0
    | n < 0 = negative (Num.fromRat'' (-125) 24 (-n) d)
    | otherwise = Num.fromRat'' (-125) 24 n d

instance From Double Rational where
  from :: Double -> Rational
  from (D# x#) = case integerDecodeDouble# x# of
    (# m, e# #)
      | isTrue# (e# >=# 0#) ->
          Ratio (integerShiftL# m (int2Word# e#)) 1
      | isTrue# ((integerToWord# m `and#` 1##) `eqWord#` 0##) ->
          case elimZerosInteger m (negateInt# e#) of
            (# n, d# #) -> Ratio n (integerShiftL# 1 (int2Word# d#))
      | otherwise ->
          Ratio m (integerShiftL# 1 (int2Word# (negateInt# e#)))
instance From Natural Double where
  from :: Natural -> Double
  from = Num.fromIntegral
instance From Int Double where
  from :: Int -> Double
  from = Num.fromIntegral
instance From Integer Double where
  from :: Integer -> Double
  from = Num.fromIntegral
instance From Ration Double where
  from :: Ration -> Double
  {-# NOINLINE [0] from #-}
  from (Ratio n 0)
    | n == 0 = 0 Num./ 0
    | n < 0 = (-1) Num./ 0
    | otherwise = 1 Num./ 0
  from (Ratio n d)
    | n == 0 = Num.encodeFloat 0 0
    | n < 0 = negative (Num.fromRat'' (-1021) 53 (negative (from n)) (from d))
    | otherwise = Num.fromRat'' (-1021) 53 (from n) (from d)
instance From Rational Double where
  from :: Rational -> Double
  {-# NOINLINE [0] from #-}
  from (Ratio n 0)
    | n == 0 = 0 Num./ 0
    | n < 0 = (-1) Num./ 0
    | otherwise = 1 Num./ 0
  from (Ratio n d)
    | n == 0 = Num.encodeFloat 0 0
    | n < 0 = negative (Num.fromRat'' (-1021) 53 (-n) d)
    | otherwise = Num.fromRat'' (-1021) 53 n d

instance (Additive y, From y x) => From y (Complex x) where
  from :: y -> Complex x
  from n = from n :+ from @y zero

instance (HasResolution e) => From Natural (Fixed e) where
  from :: Natural -> Fixed e
  from = Num.fromIntegral
instance (HasResolution e) => From Int (Fixed e) where
  from :: Int -> Fixed e
  from = Num.fromIntegral
instance (HasResolution e) => From Integer (Fixed e) where
  from :: Integer -> Fixed e
  from = Num.fromIntegral
instance (HasResolution e) => From Rational (Fixed e) where
  from :: Rational -> Fixed e
  from = Num.fromRational . toDataRational
instance (HasResolution e) => From (Fixed e) Rational where
  from :: Fixed e -> Rational
  from = fromDataRational . Num.toRational

instance (From Natural x) => From Natural (List1 x) where
  from :: Natural -> List1 x
  from = Sole . from
instance (From Int x) => From Int (List1 x) where
  from :: Int -> List1 x
  from = Sole . from
instance (From Integer x) => From Integer (List1 x) where
  from :: Integer -> List1 x
  from = Sole . from
instance (From Rational x) => From Rational (List1 x) where
  from :: Rational -> List1 x
  from = Sole . from

instance (From Natural x) => From Natural (Projective x) where
  from :: Natural -> Projective x
  from = Projective . from
instance (From Int x) => From Int (Projective x) where
  from :: Int -> Projective x
  from = Projective . from
instance (From Integer x) => From Integer (Projective x) where
  from :: Integer -> Projective x
  from = Projective . from
instance (From Rational x) => From Rational (Projective x) where
  from :: Rational -> Projective x
  from = Projective . from

instance (From Natural x) => From Natural (Tropical x) where
  from :: Natural -> Tropical x
  from = Tropical . from
instance (From Int x) => From Int (Tropical x) where
  from :: Int -> Tropical x
  from = Tropical . from
instance (From Integer x) => From Integer (Tropical x) where
  from :: Integer -> Tropical x
  from = Tropical . from
instance (From Rational x) => From Rational (Tropical x) where
  from :: Rational -> Tropical x
  from = Tropical . from

instance (From Natural x) => From Natural (Suspension x) where
  from :: Natural -> Suspension x
  from = Meridian . from
instance (From Int x) => From Int (Suspension x) where
  from :: Int -> Suspension x
  from = Meridian . from
instance (From Integer x) => From Integer (Suspension x) where
  from :: Integer -> Suspension x
  from = Meridian . from
instance (From Rational x) => From Rational (Suspension x) where
  from :: Rational -> Suspension x
  from = Meridian . from

instance (KnownNat n) => From (Finite n) Natural where
  from :: Finite n -> Natural
  from = from . getFinite
instance (KnownNat n) => From (Finite n) Integer where
  from :: Finite n -> Integer
  from = getFinite
instance (KnownNat n) => From (Finite n) Int where
  from :: Finite n -> Int
  from = from . getFinite
instance (KnownNat n) => From Natural (Finite n) where
  from :: Natural -> Finite n
  from n = modulo (from n)
instance (KnownNat n) => From Integer (Finite n) where
  from :: Integer -> Finite n
  from = modulo
instance (KnownNat n) => From Int (Finite n) where
  from :: Int -> Finite n
  from i = modulo (from i)

-- Absolute

class Absolute x y where
  absolute :: x -> y

instance Absolute Int8 Int8 where
  absolute :: Int8 -> Int8
  absolute x
    | x < zero = negative x
    | otherwise = x

instance Absolute Int8 Word8 where
  absolute :: Int8 -> Word8
  absolute x
    | x < zero = Num.fromIntegral (negative x)
    | otherwise = Num.fromIntegral x

instance Absolute Word8 Word8 where
  absolute :: Word8 -> Word8
  absolute x = x

instance Absolute Int16 Int16 where
  absolute :: Int16 -> Int16
  absolute x
    | x < zero = negative x
    | otherwise = x

instance Absolute Int16 Word16 where
  absolute :: Int16 -> Word16
  absolute x
    | x < zero = Num.fromIntegral (negative x)
    | otherwise = Num.fromIntegral x

instance Absolute Word16 Word16 where
  absolute :: Word16 -> Word16
  absolute x = x

instance Absolute Int32 Int32 where
  absolute :: Int32 -> Int32
  absolute x
    | x < zero = negative x
    | otherwise = x

instance Absolute Int32 Word32 where
  absolute :: Int32 -> Word32
  absolute x
    | x < zero = Num.fromIntegral (negative x)
    | otherwise = Num.fromIntegral x

instance Absolute Word32 Word32 where
  absolute :: Word32 -> Word32
  absolute x = x

instance Absolute Int64 Int64 where
  absolute :: Int64 -> Int64
  absolute x
    | x < zero = negative x
    | otherwise = x

instance Absolute Int64 Word64 where
  absolute :: Int64 -> Word64
  absolute x
    | x < zero = Num.fromIntegral (negative x)
    | otherwise = Num.fromIntegral x

instance Absolute Word64 Word64 where
  absolute :: Word64 -> Word64
  absolute x = x

instance Absolute Int Int where
  absolute :: Int -> Int
  absolute x
    | x < zero = negative x
    | otherwise = x
instance Absolute Int Word where
  absolute :: Int -> Word
  absolute = Num.fromIntegral . absolute @Int @Int
instance Absolute Int Natural where
  absolute :: Int -> Natural
  absolute = Num.fromIntegral . absolute @Int @Int

instance Absolute Natural Natural where
  absolute :: Natural -> Natural
  absolute n = n

instance Absolute Integer Integer where
  absolute :: Integer -> Integer
  absolute x
    | x < zero = negative x
    | otherwise = x
instance Absolute Integer Natural where
  absolute :: Integer -> Natural
  absolute = Num.fromIntegral . absolute @Integer @Integer

instance Absolute Float Float where
  absolute :: Float -> Float
  absolute x
    | x < zero = negative x
    | x == (-0.0) = negative x
    | otherwise = x

instance Absolute Double Double where
  absolute :: Double -> Double
  absolute x
    | x < zero = negative x
    | x == (-0.0) = negative x
    | otherwise = x

instance (HasResolution e) => Absolute (Fixed e) (Fixed e) where
  absolute :: Fixed e -> Fixed e
  absolute x
    | x < zero = negative x
    | otherwise = x

-- Signed

data Sign
  = Negative
  | Unsigned
  | Positive
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

class (Ord x) => Signed x where
  sign :: x -> Sign

signum :: (Signed x, From Integer x) => x -> x
signum x = from @Integer case sign x of
  Negative -> negative one
  Unsigned -> zero
  Positive -> one

instance Signed Int8 where
  sign :: Int8 -> Sign
  sign x = case compare x zero of
    LT -> Negative
    EQ -> Unsigned
    GT -> Positive
instance Signed Int16 where
  sign :: Int16 -> Sign
  sign x = case compare x zero of
    LT -> Negative
    EQ -> Unsigned
    GT -> Positive
instance Signed Int32 where
  sign :: Int32 -> Sign
  sign x = case compare x zero of
    LT -> Negative
    EQ -> Unsigned
    GT -> Positive
instance Signed Int64 where
  sign :: Int64 -> Sign
  sign x = case compare x zero of
    LT -> Negative
    EQ -> Unsigned
    GT -> Positive
instance Signed Int where
  sign :: Int -> Sign
  sign x = case compare x zero of
    LT -> Negative
    EQ -> Unsigned
    GT -> Positive
instance Signed Word8 where
  sign :: Word8 -> Sign
  sign x = case compare x zero of
    EQ -> Unsigned
    _ -> Positive
instance Signed Word16 where
  sign :: Word16 -> Sign
  sign x = case compare x zero of
    EQ -> Unsigned
    _ -> Positive
instance Signed Word32 where
  sign :: Word32 -> Sign
  sign x = case compare x zero of
    EQ -> Unsigned
    _ -> Positive
instance Signed Word64 where
  sign :: Word64 -> Sign
  sign x = case compare x zero of
    EQ -> Unsigned
    _ -> Positive
instance Signed Word where
  sign :: Word -> Sign
  sign x = case compare x zero of
    EQ -> Unsigned
    _ -> Positive
instance Signed Integer where
  sign :: Integer -> Sign
  sign x = case compare x zero of
    LT -> Negative
    EQ -> Unsigned
    GT -> Positive
instance Signed Natural where
  sign :: Natural -> Sign
  sign x = case compare x zero of
    EQ -> Unsigned
    _ -> Positive
instance Signed Float where
  sign :: Float -> Sign
  sign x = case compare x zero of
    LT -> Negative
    EQ -> Unsigned
    GT -> Positive
instance Signed Double where
  sign :: Double -> Sign
  sign x = case compare x zero of
    LT -> Negative
    EQ -> Unsigned
    GT -> Positive
instance (Signed x) => Signed (Suspension x) where
  sign :: Suspension x -> Sign
  sign = \case
    South -> Negative
    Meridian x -> sign x
    North -> Positive
instance (Signed x) => Signed (Projective x) where
  sign :: Projective x -> Sign
  sign = \case
    Infinity -> Unsigned
    Projective x -> sign x
instance (HasResolution e) => Signed (Fixed e) where
  sign :: Fixed e -> Sign
  sign x = case compare x zero of
    LT -> Negative
    EQ -> Unsigned
    GT -> Positive

-- Conjugate

class Conjugate x where
  conjugate :: x -> x

instance Conjugate Int where
  conjugate :: Int -> Int
  conjugate x = x
instance Conjugate Integer where
  conjugate :: Integer -> Integer
  conjugate x = x
instance Conjugate Rational where
  conjugate :: Rational -> Rational
  conjugate x = x
instance Conjugate Float where
  conjugate :: Float -> Float
  conjugate x = x
instance Conjugate Double where
  conjugate :: Double -> Double
  conjugate x = x
instance (AdditiveGroup x) => Conjugate (Complex x) where
  conjugate :: Complex x -> Complex x
  conjugate (a :+ bi) = a :+ negative bi

-- Addition

infixl 6 +.
infixl 6 +

class Addition x y z where
  (+.) :: x -> y -> z

(+) :: (Addition x x x) => x -> x -> x
(+) = (+.)

instance Addition Int8 Int8 Int8 where
  (+.) :: Int8 -> Int8 -> Int8
  (+.) = (Num.+)

instance Addition Word8 Word8 Word8 where
  (+.) :: Word8 -> Word8 -> Word8
  (+.) = (Num.+)

instance Addition Int16 Int16 Int16 where
  (+.) :: Int16 -> Int16 -> Int16
  (+.) = (Num.+)

instance Addition Word16 Word16 Word16 where
  (+.) :: Word16 -> Word16 -> Word16
  (+.) = (Num.+)

instance Addition Int32 Int32 Int32 where
  (+.) :: Int32 -> Int32 -> Int32
  (+.) = (Num.+)

instance Addition Word32 Word32 Word32 where
  (+.) :: Word32 -> Word32 -> Word32
  (+.) = (Num.+)

instance Addition Int64 Int64 Int64 where
  (+.) :: Int64 -> Int64 -> Int64
  (+.) = (Num.+)

instance Addition Word64 Word64 Word64 where
  (+.) :: Word64 -> Word64 -> Word64
  (+.) = (Num.+)

instance Addition Word Word Word where
  (+.) :: Word -> Word -> Word
  (+.) = (Num.+)

instance Addition Int Int Int where
  (+.) :: Int -> Int -> Int
  (+.) = (Num.+)

instance Addition Natural Natural Natural where
  (+.) :: Natural -> Natural -> Natural
  (+.) = (Num.+)

instance Addition Integer Integer Integer where
  (+.) :: Integer -> Integer -> Integer
  (+.) = (Num.+)
instance Addition Int Integer Integer where
  (+.) :: Int -> Integer -> Integer
  i +. i_ = Num.fromIntegral @_ @Integer i + i_
instance Addition Natural Integer Integer where
  (+.) :: Natural -> Integer -> Integer
  i +. i_ = Num.fromIntegral @_ @Integer i + i_

instance Addition Rational Rational Rational where
  (+.) :: Rational -> Rational -> Rational
  Ratio m n +. Ratio p q = reduce ((m * q) + (n * p)) (n * q)
instance Addition Int Rational Rational where
  (+.) :: Int -> Rational -> Rational
  i +. r = from i + r
instance Addition Natural Rational Rational where
  (+.) :: Natural -> Rational -> Rational
  i +. r = from i + r
instance Addition Integer Rational Rational where
  (+.) :: Integer -> Rational -> Rational
  i +. r = from i + r

instance Addition Ration Ration Ration where
  (+.) :: Ration -> Ration -> Ration
  Ratio m n +. Ratio p q = reduce ((m * q) + (n * p)) (n * q)
instance Addition Natural Ration Ration where
  (+.) :: Natural -> Ration -> Ration
  m +. r = from m + r

instance Addition Float Float Float where
  (+.) :: Float -> Float -> Float
  (+.) = (Num.+)
instance Addition Int Float Float where
  (+.) :: Int -> Float -> Float
  i +. d = Num.fromIntegral @_ @Float i + d
instance Addition Natural Float Float where
  (+.) :: Natural -> Float -> Float
  i +. d = Num.fromIntegral @_ @Float i + d
instance Addition Integer Float Float where
  (+.) :: Integer -> Float -> Float
  i +. d = Num.fromIntegral @_ @Float i + d

instance Addition Double Double Double where
  (+.) :: Double -> Double -> Double
  (+.) = (Num.+)
instance Addition Int Double Double where
  (+.) :: Int -> Double -> Double
  i +. d = Num.fromIntegral @_ @Double i + d
instance Addition Natural Double Double where
  (+.) :: Natural -> Double -> Double
  i +. d = Num.fromIntegral @_ @Double i + d
instance Addition Integer Double Double where
  (+.) :: Integer -> Double -> Double
  i +. d = Num.fromIntegral @_ @Double i + d
instance Addition Float Double Double where
  (+.) :: Float -> Double -> Double
  f +. d = Num.realToFrac @_ @Double f + d

instance (Addition x x x) => Addition (Complex x) (Complex x) (Complex x) where
  (+.) :: Complex x -> Complex x -> Complex x
  (x :+ yi) +. (a :+ bi) = (x + a) :+ (yi + bi)

instance
  (Additive x) =>
  Addition (Suspension x) (Suspension x) (Suspension x)
  where
  (+.) :: Suspension x -> Suspension x -> Suspension x
  (+.) = \cases
    North South -> zero
    South North -> zero
    South _ -> South
    North _ -> North
    _ South -> South
    _ North -> North
    (Meridian x) (Meridian y) -> Meridian (x + y)

instance
  (Addition x x x) =>
  Addition (Projective x) (Projective x) (Projective x)
  where
  (+.) :: Projective x -> Projective x -> Projective x
  (+.) = \cases
    Infinity _ -> Infinity
    _ Infinity -> Infinity
    (Projective x) (Projective y) -> Projective (x + y)

instance (Ord x) => Addition (Tropical x) (Tropical x) (Tropical x) where
  (+.) :: Tropical x -> Tropical x -> Tropical x
  (+.) = \cases
    Pole x -> x
    x Pole -> x
    (Tropical x) (Tropical y) -> Tropical (min x y)

instance (HasResolution e) => Addition (Fixed e) (Fixed e) (Fixed e) where
  (+.) :: Fixed e -> Fixed e -> Fixed e
  (+.) = (Num.+)
instance (HasResolution e) => Addition Int (Fixed e) (Fixed e) where
  (+.) :: Int -> Fixed e -> Fixed e
  i +. f = from i + f
instance (HasResolution e) => Addition Natural (Fixed e) (Fixed e) where
  (+.) :: Natural -> Fixed e -> Fixed e
  n +. f = from n + f
instance (HasResolution e) => Addition Integer (Fixed e) (Fixed e) where
  (+.) :: Integer -> Fixed e -> Fixed e
  i +. f = from i + f
instance (HasResolution e) => Addition Float (Fixed e) (Fixed e) where
  (+.) :: Float -> Fixed e -> Fixed e
  d +. f = Num.realToFrac @_ @(Fixed e) d + f
instance (HasResolution e) => Addition Double (Fixed e) (Fixed e) where
  (+.) :: Double -> Fixed e -> Fixed e
  d +. f = Num.realToFrac @_ @(Fixed e) d + f

poly :: (Eq x, Additive x) => List1 x -> List1 x
poly xs = case dropWhile (== zero) (reverse xs) of
  Nothing -> Sole zero
  Just ys -> reverse ys

instance (Eq x, Additive x) => Addition (List1 x) (List1 x) (List1 x) where
  (+.) :: List1 x -> List1 x -> List1 x
  (+.) =
    (poly .) . \cases
      (Sole x) (Sole y) -> Sole (x + y)
      (Sole x) (y :|| xs) -> x + y :|| xs
      (x :|| xs) (Sole y) -> x + y :|| xs
      (x :|| xs) (y :|| ys) -> x + y :|| xs + ys

instance (Addition x x x) => Addition (Sum x) (Sum x) (Sum x) where
  (+.) :: Sum x -> Sum x -> Sum x
  Sum x +. Sum y = Sum (x + y)

instance Addition DiffTime DiffTime DiffTime where
  (+.) :: DiffTime -> DiffTime -> DiffTime
  Picoseconds x +. Picoseconds y = Picoseconds (x + y)

instance Addition NominalDiffTime NominalDiffTime NominalDiffTime where
  (+.) :: NominalDiffTime -> NominalDiffTime -> NominalDiffTime
  Nominal d0 +. Nominal d1 = Nominal (d0 + d1)

instance Addition NominalDiffTime UTCTime UTCTime where
  (+.) :: NominalDiffTime -> UTCTime -> UTCTime
  (+.) = addUTCTime
instance Addition UTCTime NominalDiffTime UTCTime where
  (+.) :: UTCTime -> NominalDiffTime -> UTCTime
  (+.) = flip addUTCTime

instance (Addition x x x, Addition y y y) => Addition (x, y) (x, y) (x, y) where
  (+.) :: (x, y) -> (x, y) -> (x, y)
  (x0, y0) +. (x1, y1) = (x0 + x1, y0 + y1)

-- Additive

class (Addition x x x) => Additive x where
  zero :: x

instance Additive Int8 where
  zero :: Int8
  zero = 0
instance Additive Word8 where
  zero :: Word8
  zero = 0
instance Additive Int16 where
  zero :: Int16
  zero = 0
instance Additive Word16 where
  zero :: Word16
  zero = 0
instance Additive Int32 where
  zero :: Int32
  zero = 0
instance Additive Word32 where
  zero :: Word32
  zero = 0
instance Additive Int64 where
  zero :: Int64
  zero = 0
instance Additive Word64 where
  zero :: Word64
  zero = 0
instance Additive Word where
  zero :: Word
  zero = 0
instance Additive Int where
  zero :: Int
  zero = 0
instance Additive Natural where
  zero :: Natural
  zero = 0
instance Additive Integer where
  zero :: Integer
  zero = 0
instance Additive Rational where
  zero :: Rational
  zero = 0
instance Additive Ration where
  zero :: Ration
  zero = Ratio zero one
instance Additive Float where
  zero :: Float
  zero = 0
instance Additive Double where
  zero :: Double
  zero = 0
instance (Additive x) => Additive (Complex x) where
  zero :: Complex x
  zero = zero :+ zero
instance (Additive x) => Additive (Suspension x) where
  zero :: Suspension x
  zero = Meridian zero
instance (Additive x) => Additive (Projective x) where
  zero :: Projective x
  zero = Projective zero
instance (Ord x) => Additive (Tropical x) where
  zero :: Tropical x
  zero = Pole
instance (HasResolution e) => Additive (Fixed e) where
  zero :: Fixed e
  zero = 0
instance (Eq x, Additive x) => Additive (List1 x) where
  zero :: List1 x
  zero = Sole zero
instance (Additive x) => Additive (Sum x) where
  zero :: Sum x
  zero = Sum zero
instance Additive DiffTime where
  zero :: DiffTime
  zero = Picoseconds zero
instance Additive NominalDiffTime where
  zero :: NominalDiffTime
  zero = Nominal zero

instance (Additive x, Additive y) => Additive (x, y) where
  zero :: (x, y)
  zero = (zero, zero)

-- AdditiveAbelian

class (Additive x) => AdditiveAbelian x

instance AdditiveAbelian Int8
instance AdditiveAbelian Word8
instance AdditiveAbelian Int16
instance AdditiveAbelian Word16
instance AdditiveAbelian Int32
instance AdditiveAbelian Word32
instance AdditiveAbelian Int64
instance AdditiveAbelian Word64
instance AdditiveAbelian Int
instance AdditiveAbelian Natural
instance AdditiveAbelian Integer
instance AdditiveAbelian Rational
instance AdditiveAbelian Ration
instance AdditiveAbelian Float
instance AdditiveAbelian Double
instance (AdditiveAbelian x) => AdditiveAbelian (Complex x)
instance (AdditiveAbelian x) => AdditiveAbelian (Suspension x)
instance (AdditiveAbelian x) => AdditiveAbelian (Projective x)
instance (Ord x) => AdditiveAbelian (Tropical x)
instance (HasResolution e) => AdditiveAbelian (Fixed e)
instance (Eq x, AdditiveAbelian x) => AdditiveAbelian (List1 x)
instance (AdditiveAbelian x) => AdditiveAbelian (Sum x)
instance AdditiveAbelian DiffTime
instance AdditiveAbelian NominalDiffTime
instance (AdditiveAbelian x, AdditiveAbelian y) => AdditiveAbelian (x, y)

-- Subtraction

infixl 6 -.
infixl 6 -

class Subtraction x y z where
  (-.) :: x -> y -> z

(-) :: (Subtraction x x x) => x -> x -> x
(-) = (-.)

instance Subtraction Int8 Int8 Int8 where
  (-.) :: Int8 -> Int8 -> Int8
  (-.) = (Num.-)
instance Subtraction Word8 Word8 Word8 where
  (-.) :: Word8 -> Word8 -> Word8
  (-.) = (Num.-)
instance Subtraction Int16 Int16 Int16 where
  (-.) :: Int16 -> Int16 -> Int16
  (-.) = (Num.-)
instance Subtraction Word16 Word16 Word16 where
  (-.) :: Word16 -> Word16 -> Word16
  (-.) = (Num.-)
instance Subtraction Int32 Int32 Int32 where
  (-.) :: Int32 -> Int32 -> Int32
  (-.) = (Num.-)
instance Subtraction Word32 Word32 Word32 where
  (-.) :: Word32 -> Word32 -> Word32
  (-.) = (Num.-)
instance Subtraction Int64 Int64 Int64 where
  (-.) :: Int64 -> Int64 -> Int64
  (-.) = (Num.-)
instance Subtraction Word64 Word64 Word64 where
  (-.) :: Word64 -> Word64 -> Word64
  (-.) = (Num.-)

instance Subtraction Word Word Word where
  (-.) :: Word -> Word -> Word
  (-.) = (Num.-)

instance Subtraction Int Int Int where
  (-.) :: Int -> Int -> Int
  (-.) = (Num.-)
instance Subtraction Int Double Double where
  (-.) :: Int -> Double -> Double
  i -. d = from i - d

instance Subtraction Natural Natural Natural where
  (-.) :: Natural -> Natural -> Natural
  (-.) = (Num.-)
instance Subtraction Natural Natural Integer where
  (-.) :: Natural -> Natural -> Integer
  (-.) = on (-) from
instance Subtraction Natural Integer Integer where
  (-.) :: Natural -> Integer -> Integer
  n -. i = from n - i
instance Subtraction Natural Rational Rational where
  (-.) :: Natural -> Rational -> Rational
  n -. r = from n - r
instance Subtraction Natural Double Double where
  (-.) :: Natural -> Double -> Double
  n -. d = from n - d

instance Subtraction Integer Integer Integer where
  (-.) :: Integer -> Integer -> Integer
  (-.) = (Num.-)
instance Subtraction Integer Int Integer where
  (-.) :: Integer -> Int -> Integer
  i -. n = i - from n
instance Subtraction Integer Natural Integer where
  (-.) :: Integer -> Natural -> Integer
  i -. n = i - from n
instance Subtraction Integer Rational Rational where
  (-.) :: Integer -> Rational -> Rational
  i -. r = from i - r
instance Subtraction Integer Double Double where
  (-.) :: Integer -> Double -> Double
  i -. d = from i - d

instance Subtraction Rational Rational Rational where
  (-.) :: Rational -> Rational -> Rational
  Ratio m n -. Ratio p q = reduce ((m * q) - (n * p)) (n * q)
instance Subtraction Rational Int Rational where
  (-.) :: Rational -> Int -> Rational
  r -. n = r - from n
instance Subtraction Rational Natural Rational where
  (-.) :: Rational -> Natural -> Rational
  r -. n = r - from n
instance Subtraction Rational Integer Rational where
  (-.) :: Rational -> Integer -> Rational
  r -. n = r - from n
instance Subtraction Rational Double Double where
  (-.) :: Rational -> Double -> Double
  r -. d = from r - d

instance Subtraction Float Float Float where
  (-.) :: Float -> Float -> Float
  (-.) = (Num.-)
instance Subtraction Float Int Float where
  (-.) :: Float -> Int -> Float
  d -. i = d - from i
instance Subtraction Float Natural Float where
  (-.) :: Float -> Natural -> Float
  d -. i = d - from i
instance Subtraction Float Integer Float where
  (-.) :: Float -> Integer -> Float
  d -. i = d - from i

instance Subtraction Double Double Double where
  (-.) :: Double -> Double -> Double
  (-.) = (Num.-)
instance Subtraction Double Int Double where
  (-.) :: Double -> Int -> Double
  d -. i = d - from i
instance Subtraction Double Natural Double where
  (-.) :: Double -> Natural -> Double
  d -. i = d - from i
instance Subtraction Double Integer Double where
  (-.) :: Double -> Integer -> Double
  d -. i = d - from i

instance
  (Subtraction x x x) =>
  Subtraction (Complex x) (Complex x) (Complex x)
  where
  (-.) :: Complex x -> Complex x -> Complex x
  (x :+ yi) -. (a :+ bi) = (x - a) :+ (yi - bi)

instance
  (Additive x, Subtraction x x x) =>
  Subtraction (Suspension x) (Suspension x) (Suspension x)
  where
  (-.) :: Suspension x -> Suspension x -> Suspension x
  (-.) = \cases
    South South -> zero
    North North -> zero
    South _ -> South
    _ South -> North
    North _ -> North
    _ North -> South
    (Meridian x) (Meridian y) -> Meridian (x - y)

instance
  (Subtraction x x x) =>
  Subtraction (Projective x) (Projective x) (Projective x)
  where
  (-.) :: Projective x -> Projective x -> Projective x
  (-.) = \cases
    Infinity _ -> Infinity
    _ Infinity -> Infinity
    (Projective x) (Projective y) -> Projective (x - y)

instance (HasResolution e) => Subtraction (Fixed e) (Fixed e) (Fixed e) where
  (-.) :: Fixed e -> Fixed e -> Fixed e
  (-.) = (Num.-)
instance (HasResolution e) => Subtraction (Fixed e) Int (Fixed e) where
  (-.) :: Fixed e -> Int -> Fixed e
  f -. i = f - from i
instance (HasResolution e) => Subtraction (Fixed e) Natural (Fixed e) where
  (-.) :: Fixed e -> Natural -> Fixed e
  f -. n = f - from n
instance (HasResolution e) => Subtraction (Fixed e) Integer (Fixed e) where
  (-.) :: Fixed e -> Integer -> Fixed e
  f -. i = f - from i
instance (HasResolution e) => Subtraction (Fixed e) Float (Fixed e) where
  (-.) :: Fixed e -> Float -> Fixed e
  f -. d = f - Num.realToFrac @_ @(Fixed e) d
instance (HasResolution e) => Subtraction (Fixed e) Double (Fixed e) where
  (-.) :: Fixed e -> Double -> Fixed e
  f -. d = f - Num.realToFrac @_ @(Fixed e) d

instance
  (Eq x, AdditiveGroup x) =>
  Subtraction (List1 x) (List1 x) (List1 x)
  where
  (-.) :: List1 x -> List1 x -> List1 x
  (-.) =
    (poly .) . \cases
      (Sole x) (Sole y) -> Sole (x - y)
      (Sole x) (y :|| ys) -> x - y :|| morphism negative ys
      (x :|| xs) (Sole y) -> x - y :|| xs
      (x :|| xs) (y :|| ys) -> x - y :|| xs - ys

instance (Eq x, Subtraction x x x) => Subtraction (Sum x) (Sum x) (Sum x) where
  (-.) :: Sum x -> Sum x -> Sum x
  Sum x -. Sum y = Sum (x - y)

instance Subtraction DiffTime DiffTime DiffTime where
  (-.) :: DiffTime -> DiffTime -> DiffTime
  Picoseconds x -. Picoseconds y = Picoseconds (x - y)

instance Subtraction NominalDiffTime NominalDiffTime NominalDiffTime where
  (-.) :: NominalDiffTime -> NominalDiffTime -> NominalDiffTime
  Nominal x -. Nominal y = Nominal (x - y)

instance Subtraction UTCTime UTCTime NominalDiffTime where
  (-.) :: UTCTime -> UTCTime -> NominalDiffTime
  (-.) = diffUTCTime

instance (Subtraction x x x, Subtraction y y y) => Subtraction (x, y) (x, y) (x, y) where
  (-.) :: (x, y) -> (x, y) -> (x, y)
  (x0, y0) -. (x1, y1) = (x0 - x1, y0 - y1)

-- AdditiveGroup

class (Additive x, Subtraction x x x) => AdditiveGroup x where
  negative :: x -> x

instance AdditiveGroup Int8 where
  negative :: Int8 -> Int8
  negative = Num.negate
instance AdditiveGroup Int16 where
  negative :: Int16 -> Int16
  negative = Num.negate
instance AdditiveGroup Int32 where
  negative :: Int32 -> Int32
  negative = Num.negate
instance AdditiveGroup Int64 where
  negative :: Int64 -> Int64
  negative = Num.negate
instance AdditiveGroup Int where
  negative :: Int -> Int
  negative = Num.negate
instance AdditiveGroup Integer where
  negative :: Integer -> Integer
  negative = Num.negate
instance AdditiveGroup Rational where
  negative :: Rational -> Rational
  negative (Ratio m n) = Ratio (negative m) n
instance AdditiveGroup Float where
  negative :: Float -> Float
  negative = Num.negate
instance AdditiveGroup Double where
  negative :: Double -> Double
  negative = Num.negate
instance (AdditiveGroup x) => AdditiveGroup (Complex x) where
  negative :: Complex x -> Complex x
  negative (a :+ bi) = negative a :+ negative bi
instance (AdditiveGroup x) => AdditiveGroup (Suspension x) where
  negative :: Suspension x -> Suspension x
  negative = \case
    South -> North
    Meridian x -> Meridian (negative x)
    North -> South
instance (AdditiveGroup x) => AdditiveGroup (Projective x) where
  negative :: Projective x -> Projective x
  negative = \case
    Infinity -> Infinity
    Projective x -> Projective (negative x)
instance (HasResolution e) => AdditiveGroup (Fixed e) where
  negative :: Fixed e -> Fixed e
  negative = Num.negate
instance (Eq x, AdditiveGroup x) => AdditiveGroup (List1 x) where
  negative :: List1 x -> List1 x
  negative = morphism negative
instance (Eq x, AdditiveGroup x) => AdditiveGroup (Sum x) where
  negative :: Sum x -> Sum x
  negative (Sum x) = Sum (negative x)
instance AdditiveGroup DiffTime where
  negative :: DiffTime -> DiffTime
  negative (Picoseconds x) = Picoseconds (negative x)
instance AdditiveGroup NominalDiffTime where
  negative :: NominalDiffTime -> NominalDiffTime
  negative (Nominal x) = Nominal (negative x)

instance (AdditiveGroup x, AdditiveGroup y) => AdditiveGroup (x, y) where
  negative :: (x, y) -> (x, y)
  negative (x, y) = (negative x, negative y)

type AdditiveAbelianGroup = C2 AdditiveAbelian AdditiveGroup

-- Multiplication

infixl 7 *.
infixl 7 *

class Multiplication x y z where
  (*.) :: x -> y -> z

(*) :: (Multiplication x x x) => x -> x -> x
(*) = (*.)

instance Multiplication Int8 Int8 Int8 where
  (*.) :: Int8 -> Int8 -> Int8
  (*.) = (Num.*)

instance Multiplication Word8 Word8 Word8 where
  (*.) :: Word8 -> Word8 -> Word8
  (*.) = (Num.*)

instance Multiplication Int16 Int16 Int16 where
  (*.) :: Int16 -> Int16 -> Int16
  (*.) = (Num.*)

instance Multiplication Word16 Word16 Word16 where
  (*.) :: Word16 -> Word16 -> Word16
  (*.) = (Num.*)

instance Multiplication Int32 Int32 Int32 where
  (*.) :: Int32 -> Int32 -> Int32
  (*.) = (Num.*)

instance Multiplication Word32 Word32 Word32 where
  (*.) :: Word32 -> Word32 -> Word32
  (*.) = (Num.*)

instance Multiplication Int64 Int64 Int64 where
  (*.) :: Int64 -> Int64 -> Int64
  (*.) = (Num.*)

instance Multiplication Word64 Word64 Word64 where
  (*.) :: Word64 -> Word64 -> Word64
  (*.) = (Num.*)

instance Multiplication Word Word Word where
  (*.) :: Word -> Word -> Word
  (*.) = (Num.*)

instance Multiplication Int Int Int where
  (*.) :: Int -> Int -> Int
  (*.) = (Num.*)
instance Multiplication Natural Int Int where
  (*.) :: Natural -> Int -> Int
  n *. i = from n * i
instance Multiplication Integer Int Int where
  (*.) :: Integer -> Int -> Int
  n *. i = from n * i

instance Multiplication Natural Natural Natural where
  (*.) :: Natural -> Natural -> Natural
  (*.) = (Num.*)

instance Multiplication Integer Integer Integer where
  (*.) :: Integer -> Integer -> Integer
  (*.) = (Num.*)
instance Multiplication Int Integer Integer where
  (*.) :: Int -> Integer -> Integer
  i *. i_ = from i * i_
instance Multiplication Natural Integer Integer where
  (*.) :: Natural -> Integer -> Integer
  n *. i = from n * i

instance Multiplication Rational Rational Rational where
  (*.) :: Rational -> Rational -> Rational
  Ratio m n *. Ratio p q = reduce (m * p) (n * q)
instance Multiplication Int Rational Rational where
  (*.) :: Int -> Rational -> Rational
  i *. r = from i * r
instance Multiplication Natural Rational Rational where
  (*.) :: Natural -> Rational -> Rational
  n *. r = from n * r
instance Multiplication Integer Rational Rational where
  (*.) :: Integer -> Rational -> Rational
  n *. r = from n * r

instance Multiplication Ration Ration Ration where
  (*.) :: Ration -> Ration -> Ration
  Ratio m n *. Ratio p q = reduce (m * p) (n * q)
instance Multiplication Rational Ration Rational where
  (*.) :: Rational -> Ration -> Rational
  r *. p = r * from p
instance Multiplication Double Ration Double where
  (*.) :: Double -> Ration -> Double
  d *. p = d * from p

instance Multiplication Float Float Float where
  (*.) :: Float -> Float -> Float
  (*.) = (Num.*)
instance Multiplication Int Float Float where
  (*.) :: Int -> Float -> Float
  n *. d = Num.fromIntegral n Num.* d
instance Multiplication Natural Float Float where
  (*.) :: Natural -> Float -> Float
  n *. d = from n Num.* d
instance Multiplication Integer Float Float where
  (*.) :: Integer -> Float -> Float
  n *. d = from n Num.* d

instance Multiplication Double Double Double where
  (*.) :: Double -> Double -> Double
  (*.) = (Num.*)
instance Multiplication Int Double Double where
  (*.) :: Int -> Double -> Double
  n *. d = Num.fromIntegral n Num.* d
instance Multiplication Natural Double Double where
  (*.) :: Natural -> Double -> Double
  n *. d = from n Num.* d
instance Multiplication Integer Double Double where
  (*.) :: Integer -> Double -> Double
  n *. d = from n Num.* d

instance
  (Multiplication x x x) =>
  Multiplication x (Complex x) (Complex x)
  where
  (*.) :: x -> Complex x -> Complex x
  r *. (x :+ yi) = (r * x) :+ (r * yi)
instance
  (Multiplication x x x) =>
  Multiplication (Complex x) x (Complex x)
  where
  (*.) :: Complex x -> x -> Complex x
  (x :+ yi) *. r = (x * r) :+ (yi * r)
instance
  (Addition x x x, Subtraction x x x, Multiplication x x x) =>
  Multiplication (Complex x) (Complex x) (Complex x)
  where
  (*.) :: Complex x -> Complex x -> Complex x
  (a :+ b) *. (c :+ d) =
    ((a * c) - (b * d)) :+ ((a * d) + (b * c))

instance
  (Signed x, Multiplication x x x) =>
  Multiplication (Suspension x) (Suspension x) (Suspension x)
  where
  (*.) :: Suspension x -> Suspension x -> Suspension x
  (*.) = \cases
    South South -> North
    South (Meridian x) -> case sign x of
      Negative -> North
      Unsigned -> Meridian x
      Positive -> South
    South North -> South
    North South -> South
    North (Meridian x) -> case sign x of
      Negative -> South
      Unsigned -> Meridian x
      Positive -> North
    North North -> North
    (Meridian x) South -> case sign x of
      Negative -> North
      Unsigned -> Meridian x
      Positive -> South
    (Meridian x) (Meridian y) -> Meridian (x * y)
    (Meridian x) North -> case sign x of
      Negative -> South
      Unsigned -> Meridian x
      Positive -> North

instance
  (Multiplication x x x) =>
  Multiplication (Projective x) (Projective x) (Projective x)
  where
  (*.) :: Projective x -> Projective x -> Projective x
  (*.) = \cases
    Infinity _ -> Infinity
    _ Infinity -> Infinity
    (Projective x) (Projective y) -> Projective (x * y)

instance (Addition x x x) => Multiplication (Tropical x) (Tropical x) (Tropical x) where
  (*.) :: Tropical x -> Tropical x -> Tropical x
  (*.) = \cases
    Pole _ -> Pole
    _ Pole -> Pole
    (Tropical x) (Tropical y) -> Tropical (x + y)

instance (HasResolution e) => Multiplication (Fixed e) (Fixed e) (Fixed e) where
  (*.) :: Fixed e -> Fixed e -> Fixed e
  (*.) = (Num.*)
instance (HasResolution e) => Multiplication Int (Fixed e) (Fixed e) where
  (*.) :: Int -> Fixed e -> Fixed e
  i *. f = from i * f
instance (HasResolution e) => Multiplication Natural (Fixed e) (Fixed e) where
  (*.) :: Natural -> Fixed e -> Fixed e
  n *. f = from n * f
instance (HasResolution e) => Multiplication Integer (Fixed e) (Fixed e) where
  (*.) :: Integer -> Fixed e -> Fixed e
  i *. f = from i * f
instance (HasResolution e) => Multiplication Float (Fixed e) (Fixed e) where
  (*.) :: Float -> Fixed e -> Fixed e
  d *. f = Num.realToFrac @_ @(Fixed e) d * f
instance (HasResolution e) => Multiplication Double (Fixed e) (Fixed e) where
  (*.) :: Double -> Fixed e -> Fixed e
  d *. f = Num.realToFrac @_ @(Fixed e) d * f

instance
  (Eq x, Additive x, Multiplicative x) =>
  Multiplication x (List1 x) (List1 x)
  where
  (*.) :: x -> List1 x -> List1 x
  k *. (x :| xs) = poly do
    (k * x)
      :| case list1 xs of
        Nothing -> []
        Just ys -> toList (k *. ys)
instance
  (Eq x, Additive x, Multiplicative x) =>
  Multiplication (List1 x) x (List1 x)
  where
  (*.) :: List1 x -> x -> List1 x
  (x :| xs) *. k = poly do
    x * k
      :| case list1 xs of
        Nothing -> []
        Just ys -> toList (ys *. k)
instance
  (Eq x, Additive x, MultiplicativeAbelian x) =>
  Multiplication (List1 x) (List1 x) (List1 x)
  where
  (*.) :: List1 x -> List1 x -> List1 x
  (*.) = \cases
    (Sole x) ys -> morphism (x *) ys
    (x :|| xs) (Sole y) -> y * x :|| y *. xs
    (x :|| xs) (y :|| ys) ->
      (x * y :|| x *. ys) + (zero @x :|| xs * (y :|| ys))

instance (Multiplication x x x) => Multiplication (Product x) (Product x) (Product x) where
  (*.) :: Product x -> Product x -> Product x
  Product x *. Product y = Product (x * y)

instance Multiplication Integer DiffTime DiffTime where
  (*.) :: Integer -> DiffTime -> DiffTime
  n *. Picoseconds p = Picoseconds (n * p)

instance Multiplication Integer NominalDiffTime NominalDiffTime where
  (*.) :: Integer -> NominalDiffTime -> NominalDiffTime
  n *. Nominal p = Nominal (n *. p)

instance
  (Multiplication x x x, Multiplication y y y) =>
  Multiplication (x, y) (x, y) (x, y)
  where
  (*.) :: (x, y) -> (x, y) -> (x, y)
  (x0, y0) *. (x1, y1) = (x0 * x1, y0 * y1)

-- Multiplicative

class (Multiplication x x x) => Multiplicative x where
  one :: x

instance Multiplicative Int8 where
  one :: Int8
  one = 1

instance Multiplicative Word8 where
  one :: Word8
  one = 1

instance Multiplicative Int16 where
  one :: Int16
  one = 1

instance Multiplicative Word16 where
  one :: Word16
  one = 1

instance Multiplicative Int32 where
  one :: Int32
  one = 1

instance Multiplicative Word32 where
  one :: Word32
  one = 1

instance Multiplicative Int64 where
  one :: Int64
  one = 1

instance Multiplicative Word64 where
  one :: Word64
  one = 1

instance Multiplicative Word where
  one :: Word
  one = 1
instance Multiplicative Int where
  one :: Int
  one = 1
instance Multiplicative Natural where
  one :: Natural
  one = 1
instance Multiplicative Integer where
  one :: Integer
  one = 1
instance Multiplicative Rational where
  one :: Rational
  one = 1
instance Multiplicative Ration where
  one :: Ration
  one = Ratio one one
instance Multiplicative Float where
  one :: Float
  one = 1
instance Multiplicative Double where
  one :: Double
  one = 1
instance
  (Additive x, Subtraction x x x, Multiplicative x) =>
  Multiplicative (Complex x)
  where
  one :: Complex x
  one = one :+ zero
instance (Signed x, Multiplicative x) => Multiplicative (Suspension x) where
  one :: Suspension x
  one = Meridian one
instance (Multiplicative x) => Multiplicative (Projective x) where
  one :: Projective x
  one = Projective one
instance (Additive x) => Multiplicative (Tropical x) where
  one :: Tropical x
  one = Tropical zero
instance (HasResolution e) => Multiplicative (Fixed e) where
  one :: Fixed e
  one = 1
instance
  (Eq x, Additive x, MultiplicativeAbelian x) =>
  Multiplicative (List1 x)
  where
  one :: List1 x
  one = Sole one
instance (Multiplicative x) => Multiplicative (Product x) where
  one :: Product x
  one = Product one

instance (Multiplicative x, Multiplicative y) => Multiplicative (x, y) where
  one :: (x, y)
  one = (one, one)

-- MultiplicativeAbelian

class (Multiplicative x) => MultiplicativeAbelian x

instance MultiplicativeAbelian Int8
instance MultiplicativeAbelian Word8
instance MultiplicativeAbelian Int16
instance MultiplicativeAbelian Word16
instance MultiplicativeAbelian Int32
instance MultiplicativeAbelian Word32
instance MultiplicativeAbelian Int64
instance MultiplicativeAbelian Word64

instance MultiplicativeAbelian Word
instance MultiplicativeAbelian Int
instance MultiplicativeAbelian Natural
instance MultiplicativeAbelian Integer
instance MultiplicativeAbelian Rational
instance MultiplicativeAbelian Ration
instance MultiplicativeAbelian Float
instance MultiplicativeAbelian Double
instance
  ( Additive x
  , Subtraction x x x
  , MultiplicativeAbelian x
  ) =>
  MultiplicativeAbelian (Complex x)
instance
  (Signed x, MultiplicativeAbelian x) =>
  MultiplicativeAbelian (Suspension x)
instance
  (MultiplicativeAbelian x) =>
  MultiplicativeAbelian (Projective x)
instance (AdditiveAbelian x) => MultiplicativeAbelian (Tropical x)
instance (HasResolution e) => MultiplicativeAbelian (Fixed e)
instance
  (Eq x, Additive x, MultiplicativeAbelian x) =>
  MultiplicativeAbelian (List1 x)
instance (MultiplicativeAbelian x) => MultiplicativeAbelian (Product x)

instance
  (MultiplicativeAbelian x, MultiplicativeAbelian y) =>
  MultiplicativeAbelian (x, y)

-- Division

infixl 7 /.
infixl 7 /

class Division x y z where
  (/.) :: x -> y -> z

(/) :: (Division x x x) => x -> x -> x
(/) = (/.)

instance Division Int Int Rational where
  (/.) :: Int -> Int -> Rational
  (/.) = on reduce from

instance Division Natural Natural Rational where
  (/.) :: Natural -> Natural -> Rational
  (/.) = on reduce from

instance Division Integer Integer Rational where
  (/.) :: Integer -> Integer -> Rational
  (/.) = reduce
instance Division Integer Int Rational where
  (/.) :: Integer -> Int -> Rational
  i /. n = i /. from @Int @Integer n
instance Division Integer Natural Rational where
  (/.) :: Integer -> Natural -> Rational
  i /. n = i /. from @Natural @Integer n

instance Division Rational Rational Rational where
  (/.) :: Rational -> Rational -> Rational
  Ratio n0 d0 /. Ratio n1 d1 = reduce (n0 * d1) (n1 * d0)
instance Division Rational Int Rational where
  (/.) :: Rational -> Int -> Rational
  r /. n = r / from n
instance Division Rational Natural Rational where
  (/.) :: Rational -> Natural -> Rational
  r /. n = r / from n
instance Division Rational Integer Rational where
  (/.) :: Rational -> Integer -> Rational
  r /. n = r / from n

instance Division Ration Ration Ration where
  (/.) :: Ration -> Ration -> Ration
  Ratio m n /. Ratio p q = reduce (m * q) (n * p)

instance Division Float Float Float where
  (/.) :: Float -> Float -> Float
  (/.) = (Num./)

instance Division Double Double Double where
  (/.) :: Double -> Double -> Double
  (/.) = (Num./)

instance
  ( Addition x x x
  , Subtraction x x x
  , Multiplication x x x
  , Division x x x
  ) =>
  Division (Complex x) (Complex x) (Complex x)
  where
  (/.) :: Complex x -> Complex x -> Complex x
  (u :+ v) /. (x :+ y) =
    ((u * x) + (v * y)) / ((x * x) + (y * y))
      :+ ((v * x) - (u * y)) / ((x * x) + (y * y))

instance
  (Signed x, AdditiveGroup x, Multiplicative x, Division x x x) =>
  Division (Suspension x) (Suspension x) (Suspension x)
  where
  (/.) :: Suspension x -> Suspension x -> Suspension x
  (/.) = \cases
    South South -> one
    South (Meridian x) -> case sign x of
      Negative -> North
      _ -> South
    South North -> negative one
    (Meridian _) South -> zero
    (Meridian x) (Meridian y) -> Meridian (x / y)
    (Meridian _) North -> zero
    North South -> negative one
    North (Meridian x) -> case sign x of
      Negative -> South
      _ -> North
    North North -> one

instance
  (Eq x, Additive x, Division x x x) =>
  Division (Projective x) (Projective x) (Projective x)
  where
  (/.) :: Projective x -> Projective x -> Projective x
  (/.) = \cases
    Infinity _ -> Infinity
    _ Infinity -> Projective zero
    (Projective x) (Projective y)
      | y == zero -> Infinity
      | otherwise -> Projective (x / y)

instance (HasResolution e) => Division (Fixed e) (Fixed e) (Fixed e) where
  (/.) :: Fixed e -> Fixed e -> Fixed e
  (/.) = (Num./)
instance (HasResolution e) => Division (Fixed e) (Fixed e) Rational where
  (/.) :: Fixed e -> Fixed e -> Rational
  (/.) = on (/) from
instance (HasResolution e) => Division (Fixed e) Int (Fixed e) where
  (/.) :: Fixed e -> Int -> Fixed e
  f /. n = f / from n
instance (HasResolution e) => Division (Fixed e) Natural (Fixed e) where
  (/.) :: Fixed e -> Natural -> Fixed e
  f /. n = f / from n
instance (HasResolution e) => Division (Fixed e) Integer (Fixed e) where
  (/.) :: Fixed e -> Integer -> Fixed e
  f /. n = f / from n
instance (Division x x x) => Division (Product x) (Product x) (Product x) where
  (/.) :: Product x -> Product x -> Product x
  Product n /. Product d = Product (n / d)

instance (Division x x x, Division y y y) => Division (x, y) (x, y) (x, y) where
  (/.) :: (x, y) -> (x, y) -> (x, y)
  (x0, y0) /. (x1, y1) = (x0 / x1, y0 / y1)

-- MultiplicativeGroup

class (Multiplicative x, Division x x x) => MultiplicativeGroup x where
  reciprocal :: x -> x

instance MultiplicativeGroup Rational where
  reciprocal :: Rational -> Rational
  reciprocal (Ratio n d) = reduce d n
instance MultiplicativeGroup Ration where
  reciprocal :: Ration -> Ration
  reciprocal (Ratio m n) = reduce n m
instance MultiplicativeGroup Float where
  reciprocal :: Float -> Float
  reciprocal = Num.recip
instance MultiplicativeGroup Double where
  reciprocal :: Double -> Double
  reciprocal = Num.recip
instance
  ( Additive x
  , Subtraction x x x
  , Multiplicative x
  , Division x x x
  ) =>
  MultiplicativeGroup (Complex x)
  where
  reciprocal :: Complex x -> Complex x
  reciprocal = (one @(Complex x) /)
instance
  (Signed x, AdditiveGroup x, MultiplicativeGroup x) =>
  MultiplicativeGroup (Suspension x)
  where
  reciprocal :: Suspension x -> Suspension x
  reciprocal = \case
    South -> zero
    Meridian x -> Meridian (reciprocal x)
    North -> zero
instance
  (Eq x, Additive x, MultiplicativeGroup x) =>
  MultiplicativeGroup (Projective x)
  where
  reciprocal :: Projective x -> Projective x
  reciprocal = \case
    Infinity -> zero
    Projective y
      | y == zero -> Infinity
      | otherwise -> Projective (reciprocal y)
instance (HasResolution e) => MultiplicativeGroup (Fixed e) where
  reciprocal :: Fixed e -> Fixed e
  reciprocal = Num.recip
instance (MultiplicativeGroup x) => MultiplicativeGroup (Product x) where
  reciprocal :: Product x -> Product x
  reciprocal (Product x) = Product (reciprocal x)

instance
  (MultiplicativeGroup x, MultiplicativeGroup y) =>
  MultiplicativeGroup (x, y)
  where
  reciprocal :: (x, y) -> (x, y)
  reciprocal (x, y) = (reciprocal x, reciprocal y)

type MultiplicativeAbelianGroup = C2 MultiplicativeAbelian MultiplicativeGroup

-- Power

infixr 8 ^

class Power x y z where
  (^) :: x -> y -> z

instance Power Int Int Rational where
  (^) :: Int -> Int -> Rational
  (^) = (Num.^^) . from
instance Power Int Natural Int where
  (^) :: Int -> Natural -> Int
  (^) = (Num.^)
instance Power Int Integer Rational where
  (^) :: Int -> Integer -> Rational
  (^) = (Num.^^) . from

instance Power Word Word Word where
  (^) :: Word -> Word -> Word
  (^) = (Num.^)
instance Power Word Natural Word where
  (^) :: Word -> Natural -> Word
  (^) = (Num.^)
instance Power Word Integer Rational where
  (^) :: Word -> Integer -> Rational
  (^) = (Num.^^) . from

instance Power Natural Int Rational where
  (^) :: Natural -> Int -> Rational
  (^) = (Num.^^) . from
instance Power Natural Natural Natural where
  (^) :: Natural -> Natural -> Natural
  (^) = (Num.^)
instance Power Natural Integer Rational where
  (^) :: Natural -> Integer -> Rational
  (^) = (Num.^^) . from

instance Power Integer Int Rational where
  (^) :: Integer -> Int -> Rational
  (^) = (Num.^^) . from
instance Power Integer Natural Integer where
  (^) :: Integer -> Natural -> Integer
  (^) = (Num.^)
instance Power Integer Integer Rational where
  (^) :: Integer -> Integer -> Rational
  (^) = (Num.^^) . from

instance Power Rational Int Rational where
  (^) :: Rational -> Int -> Rational
  (^) = (Num.^^)
instance Power Rational Natural Rational where
  (^) :: Rational -> Natural -> Rational
  (^) = (Num.^)
instance Power Rational Integer Rational where
  (^) :: Rational -> Integer -> Rational
  (^) = (Num.^^)

instance Power Float Int Float where
  (^) :: Float -> Int -> Float
  (^) = (Num.^^)
instance Power Float Natural Float where
  (^) :: Float -> Natural -> Float
  (^) = (Num.^)
instance Power Float Integer Float where
  (^) :: Float -> Integer -> Float
  (^) = (Num.^^)
instance Power Float Rational Float where
  (^) :: Float -> Rational -> Float
  r ^ Ratio n d = (r ^ n) Num.** from (one @Integer /. d :: Rational)
instance Power Float Float Float where
  (^) :: Float -> Float -> Float
  (^) = (Num.**)

instance Power Double Int Double where
  (^) :: Double -> Int -> Double
  (^) = (Num.^^)
instance Power Double Natural Double where
  (^) :: Double -> Natural -> Double
  (^) = (Num.^)
instance Power Double Integer Double where
  (^) :: Double -> Integer -> Double
  (^) = (Num.^^)
instance Power Double Rational Double where
  (^) :: Double -> Rational -> Double
  r ^ Ratio n d = (r ^ n) Num.** from (one @Integer /. d :: Rational)
instance Power Double Double Double where
  (^) :: Double -> Double -> Double
  (^) = (Num.**)

instance (HasResolution e) => Power (Fixed e) Int (Fixed e) where
  (^) :: Fixed e -> Int -> Fixed e
  (^) = (Num.^^)
instance (HasResolution e) => Power (Fixed e) Natural (Fixed e) where
  (^) :: Fixed e -> Natural -> Fixed e
  (^) = (Num.^)
instance (HasResolution e) => Power (Fixed e) Integer (Fixed e) where
  (^) :: Fixed e -> Integer -> Fixed e
  (^) = (Num.^^)

instance (Eq x, Semiring x, MultiplicativeAbelian x) => Power (List1 x) Natural (List1 x) where
  (^) :: List1 x -> Natural -> List1 x
  xs ^ n = case n of
    0 -> one @(List1 x)
    _ -> xs * (xs ^ pred n)

instance (KnownNat n) => Power (Modulo n) Natural (Modulo n) where
  (^) :: Modulo n -> Natural -> Modulo n
  a ^ n = from (powmod' 1 (from a) n (natVal (Proxy @n)))
   where
    powmod' :: Natural -> Natural -> Natural -> Natural -> Natural
    powmod' result x b ø
      | b == 0 = result `remainder` ø
      | odd b = powmod' (result * x) x (pred b) ø `remainder` ø
      | otherwise =
          powmod' result ((x * x) `remainder` ø) (b `quotient` 2) ø
            `remainder` ø

instance (Power x r x, Power y r y) => Power (x, y) r (x, y) where
  (^) :: (Power x r x, Power y r y) => (x, y) -> r -> (x, y)
  (x, y) ^ r = (x ^ r, y ^ r)

class (Field x, Power x Rational x) => Root x where
  (√) :: Natural -> x -> x
  n √ x = x ^ reduce one (from @Natural @Integer n)

instance Root Float
instance Root Double

-- Euclidean

class (Semiring x) => Euclidean x where
  euclidean :: x -> x -> (x, x)
  degree :: x -> Natural

quotient :: (Euclidean x) => x -> x -> x
quotient = (fst .) . euclidean

remainder :: (Euclidean x) => x -> x -> x
remainder = (snd .) . euclidean

even :: (Eq x, Euclidean x, Additive x) => x -> Bool
even x = remainder x (one + one) == zero

odd :: (Eq x, Euclidean x, Additive x) => x -> Bool
odd = not . even

gcd :: (Eq x, Euclidean x, Additive x) => x -> x -> x
gcd x y
  | y == zero = x
  | otherwise = gcd y (remainder x y)

lcm :: (Eq x, Euclidean x, Additive x, Multiplicative x) => x -> x -> x
lcm x y
  | x == zero = zero
  | y == zero = zero
  | otherwise = quotient x (gcd x y) * y

evenOdd :: (Eq x, Euclidean x, Additive x, Multiplicative x) => x -> (x, x)
evenOdd n
  | n == zero = (zero, one)
  | otherwise = case n `euclidean` (one + one) of
      (q, r)
        | r == zero -> morphism' (+ one) (evenOdd q)
        | otherwise -> (zero, n)

instance Euclidean Int8 where
  euclidean :: Int8 -> Int8 -> (Int8, Int8)
  euclidean = Num.quotRem
  degree :: Int8 -> Natural
  degree = Num.fromIntegral . Num.abs
instance Euclidean Word8 where
  euclidean :: Word8 -> Word8 -> (Word8, Word8)
  euclidean = Num.quotRem
  degree :: Word8 -> Natural
  degree = Num.fromIntegral
instance Euclidean Int16 where
  euclidean :: Int16 -> Int16 -> (Int16, Int16)
  euclidean = Num.quotRem
  degree :: Int16 -> Natural
  degree = Num.fromIntegral . Num.abs
instance Euclidean Word16 where
  euclidean :: Word16 -> Word16 -> (Word16, Word16)
  euclidean = Num.quotRem
  degree :: Word16 -> Natural
  degree = Num.fromIntegral
instance Euclidean Int32 where
  euclidean :: Int32 -> Int32 -> (Int32, Int32)
  euclidean = Num.quotRem
  degree :: Int32 -> Natural
  degree = Num.fromIntegral . Num.abs
instance Euclidean Word32 where
  euclidean :: Word32 -> Word32 -> (Word32, Word32)
  euclidean = Num.quotRem
  degree :: Word32 -> Natural
  degree = Num.fromIntegral
instance Euclidean Int64 where
  euclidean :: Int64 -> Int64 -> (Int64, Int64)
  euclidean = Num.quotRem
  degree :: Int64 -> Natural
  degree = Num.fromIntegral . Num.abs
instance Euclidean Word64 where
  euclidean :: Word64 -> Word64 -> (Word64, Word64)
  euclidean = Num.quotRem
  degree :: Word64 -> Natural
  degree = Num.fromIntegral
instance Euclidean Word where
  euclidean :: Word -> Word -> (Word, Word)
  euclidean = Num.quotRem
  degree :: Word -> Natural
  degree = Num.fromIntegral

instance Euclidean Int where
  euclidean :: Int -> Int -> (Int, Int)
  euclidean = Num.quotRem
  degree :: Int -> Natural
  degree = Num.fromIntegral . Num.abs
instance Euclidean Natural where
  euclidean :: Natural -> Natural -> (Natural, Natural)
  euclidean = Num.quotRem
  degree :: Natural -> Natural
  degree n = n
instance Euclidean Integer where
  euclidean :: Integer -> Integer -> (Integer, Integer)
  euclidean = Num.quotRem
  degree :: Integer -> Natural
  degree = Num.fromIntegral . Num.abs

instance Euclidean (Complex Integer) where
  euclidean ::
    Complex Integer ->
    Complex Integer ->
    (Complex Integer, Complex Integer)
  euclidean (x :+ yi) b@(c :+ di) = (q0 :+ q1, f0 :+ f1)
   where
    -- https://arxiv.org/html/2502.21136v1
    xcyd = (x * c - yi * di) /. degree b
    xdyc = (x * di + yi * c) /. degree b
    q0 = round @Rational xcyd
    q1 = round @Rational xdyc
    f0'' = xcyd -. q0 :: Rational
    f1'' = xdyc -. q1 :: Rational
    f0' :+ f1' = (f0'' :+ f1'') * (from c :+ from di)
    f0 = round f0'
    f1 = round f1'
  degree :: Complex Integer -> Natural
  degree (a :+ b) = absolute ((a * a) + (b * b))
instance (Eq x, Field x) => Euclidean (List1 x) where
  euclidean ::
    List1 x -> List1 x -> (List1 x, List1 x)
  euclidean a b =
    let d = degree b
        c = last b
     in loop (zero, a) \(q, r) ->
          let dr = degree r
              s = List.genericReplicate (dr -. d :: Integer) zero |: (last r / c)
           in if dr >= d
                then Left (q + s, r - (s * b))
                else Right (poly q, poly r)

  degree :: (Eq x, Field x) => List1 x -> Natural
  degree = \case
    Sole _ -> zero
    _ :|| xs -> one @Natural + degree xs

-- Modulo

type Modulo :: Nat -> Type
newtype Modulo n = Modulo Natural deriving (Eq, Ord, Show)

reciprocalModulo :: forall n. (KnownNat n) => Modulo n -> Maybe Natural
reciprocalModulo (Modulo a) = case natVal (Proxy @n) of
  1 | a == 0 -> Just 0
  m -> case Num.integerRecipMod# (from a) m of
    (# | () #) -> if m == 1 then Just 0 else Nothing
    (# b | #) -> Just b

instance (KnownNat n) => From Natural (Modulo n) where
  from :: Natural -> Modulo n
  from n = Modulo (n `remainder` natVal (Proxy @n))
instance (KnownNat n) => From Integer (Modulo n) where
  from :: Integer -> Modulo n
  from n
    | n < 0 =
        Modulo (natVal (Proxy @n) - (absolute n `remainder` natVal (Proxy @n)))
    | otherwise = Modulo (absolute n `remainder` natVal (Proxy @n))
instance (KnownNat n) => From (Modulo n) Natural where
  from :: Modulo n -> Natural
  from (Modulo n) = n
instance (KnownNat n) => From (Modulo n) Integer where
  from :: Modulo n -> Integer
  from (Modulo n) = from n

instance (KnownNat n) => Addition (Modulo n) (Modulo n) (Modulo n) where
  (+.) :: Modulo n -> Modulo n -> Modulo n
  Modulo a0 +. Modulo a1 = Modulo ((a0 + a1) `remainder` natVal (Proxy @n))
instance (KnownNat n) => Additive (Modulo n) where
  zero :: (KnownNat n) => Modulo n
  zero = Modulo zero
instance (KnownNat n) => AdditiveAbelian (Modulo n)
instance (KnownNat n) => Subtraction (Modulo n) (Modulo n) (Modulo n) where
  (-.) :: Modulo n -> Modulo n -> Modulo n
  Modulo a0 -. Modulo a1
    | a0 < a1 =
        Modulo (natVal (Proxy @n) - ((a1 - a0) `remainder` natVal (Proxy @n)))
    | otherwise = Modulo ((a0 - a1) `remainder` natVal (Proxy @n))
instance (KnownNat n) => AdditiveGroup (Modulo n) where
  negative :: Modulo n -> Modulo n
  negative (Modulo a) = Modulo (natVal (Proxy @n) - a `remainder` natVal (Proxy @n))

instance (KnownNat n) => Multiplication (Modulo n) (Modulo n) (Modulo n) where
  (*.) :: Modulo n -> Modulo n -> Modulo n
  Modulo a0 *. Modulo a1 = Modulo ((a0 * a1) `remainder` natVal (Proxy @n))
instance (KnownNat n) => Multiplicative (Modulo n) where
  one :: Modulo n
  one = Modulo one
instance (KnownNat n) => MultiplicativeAbelian (Modulo n)
instance (KnownNat n) => Division (Modulo n) (Modulo n) (Modulo n) where
  (/.) :: Modulo n -> Modulo n -> Modulo n
  Modulo a0 /. a1 = case reciprocalModulo a1 of
    Nothing -> GHC.throw GHC.DivideByZero
    Just a -> Modulo a0 * Modulo a
instance (KnownNat n) => MultiplicativeGroup (Modulo n) where
  reciprocal :: Modulo n -> Modulo n
  reciprocal a = case reciprocalModulo a of
    Nothing -> GHC.throw GHC.DivideByZero
    Just a0 -> Modulo a0

-- Distributive

class (Additive x, Multiplicative x) => Distributive x

instance Distributive Int8
instance Distributive Word8
instance Distributive Int16
instance Distributive Word16
instance Distributive Int32
instance Distributive Word32
instance Distributive Int64
instance Distributive Word64
instance Distributive Int
instance Distributive Word
instance Distributive Integer
instance Distributive Natural
instance Distributive Rational
instance Distributive Float
instance Distributive Double

instance
  (Additive x, Subtraction x x x, Multiplicative x) =>
  Distributive (Complex x)
instance
  (Signed x, Additive x, Multiplicative x) =>
  Distributive (Suspension x)
instance (Additive x, Multiplicative x) => Distributive (Projective x)
instance (Ord x, Additive x) => Distributive (Tropical x)
instance (HasResolution e) => Distributive (Fixed e)
instance
  (Eq x, Additive x, MultiplicativeAbelian x) =>
  Distributive (List1 x)

instance (Distributive x, Distributive y) => Distributive (x, y)

-- Semiring

class (Distributive x) => Semiring x

instance Semiring Int8
instance Semiring Word8
instance Semiring Int16
instance Semiring Word16
instance Semiring Int32
instance Semiring Word32
instance Semiring Int64
instance Semiring Word64
instance Semiring Int
instance Semiring Word
instance Semiring Integer
instance Semiring Natural

instance Semiring Rational
instance Semiring Float
instance Semiring Double
instance
  (Additive x, Subtraction x x x, Multiplicative x) =>
  Semiring (Complex x)
instance
  (Signed x, Additive x, Multiplicative x) =>
  Semiring (Suspension x)
instance (Additive x, Multiplicative x) => Semiring (Projective x)
instance (Ord x, Additive x) => Semiring (Tropical x)
instance (HasResolution e) => Semiring (Fixed e)
instance
  (Eq x, Additive x, MultiplicativeAbelian x) =>
  Semiring (List1 x)

instance (Semiring x, Semiring y) => Semiring (x, y)

-- Ring

class (Semiring x, AdditiveGroup x, AdditiveAbelian x) => Ring x

instance Ring Int8
instance Ring Int16
instance Ring Int32
instance Ring Int64
instance Ring Int
instance Ring Integer

instance Ring Rational
instance Ring Float
instance Ring Double
instance
  (AdditiveGroup x, AdditiveAbelian x, Multiplicative x) =>
  Ring (Complex x)
instance
  (Signed x, AdditiveGroup x, AdditiveAbelian x, Multiplicative x) =>
  Ring (Suspension x)
instance
  (Eq x, AdditiveGroup x, AdditiveAbelian x, Multiplicative x) =>
  Ring (Projective x)
instance (HasResolution e) => Ring (Fixed e)
instance
  (Eq x, AdditiveAbelian x, AdditiveGroup x, MultiplicativeAbelian x) =>
  Ring (List1 x)

instance (Ring x, Ring y) => Ring (x, y)

-- Domain

class (Ring x) => Domain x

instance Domain Int8
instance Domain Int16
instance Domain Int32
instance Domain Int64
instance Domain Int
instance Domain Integer

instance Domain Rational
instance Domain Float
instance Domain Double
instance
  (AdditiveGroup x, AdditiveAbelian x, Multiplicative x) =>
  Domain (Complex x)
instance (Domain x, Signed x) => Domain (Suspension x)
instance
  (Eq x, AdditiveGroup x, AdditiveAbelian x, Multiplicative x) =>
  Domain (Projective x)
instance (HasResolution e) => Domain (Fixed e)
instance (Eq x, MultiplicativeAbelian x, Domain x) => Domain (List1 x)

-- IntegralDomain

class (Domain x, MultiplicativeAbelian x) => IntegralDomain x

instance IntegralDomain Int8
instance IntegralDomain Int16
instance IntegralDomain Int32
instance IntegralDomain Int64
instance IntegralDomain Int
instance IntegralDomain Integer

instance IntegralDomain Rational
instance IntegralDomain Float
instance IntegralDomain Double
instance
  ( AdditiveGroup x
  , AdditiveAbelian x
  , MultiplicativeAbelian x
  , Division x x x
  ) =>
  IntegralDomain (Complex x)
instance (IntegralDomain x, Signed x) => IntegralDomain (Suspension x)
instance
  ( Eq x
  , AdditiveGroup x
  , AdditiveAbelian x
  , MultiplicativeAbelian x
  ) =>
  IntegralDomain (Projective x)
instance (HasResolution e) => IntegralDomain (Fixed e)
instance (Eq x, IntegralDomain x) => IntegralDomain (List1 x)

-- Field

class (IntegralDomain x, MultiplicativeGroup x) => Field x

instance Field Rational
instance Field Float
instance Field Double
instance (Field x, Division x x x) => Field (Complex x)
instance (Field x, Signed x) => Field (Suspension x)
instance
  ( Eq x
  , AdditiveGroup x
  , AdditiveAbelian x
  , MultiplicativeGroup x
  , MultiplicativeAbelian x
  ) =>
  Field (Projective x)

-- Fractional

class
  ( Eq (Integral x)
  , From (Integral x) x
  , Euclidean (Integral x)
  , Additive (Integral x)
  , Addition (Integral x) x x
  , Subtraction (Integral x) (Integral x) (Integral x)
  ) =>
  Fractional x
  where
  type Integral x
  proper :: x -> (Integral x, x)

fractional :: (Fractional x) => x -> x
fractional = snd . proper

truncate :: (Fractional x) => x -> Integral x
truncate = fst . proper

ceiling :: forall x. (Fractional x, Signed x) => x -> Integral x
ceiling x = case proper x of
  (n, r) -> case sign r of
    Positive -> n + one
    _ -> n

floor :: forall x. (Fractional x, Signed x) => x -> Integral x
floor x = case proper x of
  (n, r) -> case sign r of
    Negative -> n - one @(Integral x)
    _ -> n

round ::
  forall x.
  ( Fractional x
  , Signed x
  , From Rational x
  , Subtraction x x x
  , Absolute x x
  ) =>
  x -> Integral x
round x = case proper x of
  (n, r) ->
    let m = if sign r == Negative then n - one else n + one
     in case sign (absolute @x @x r - from @Rational (reduce 1 2)) of
          Negative -> n
          Unsigned -> if even n then n else m
          Positive -> m

instance Fractional Rational where
  type Integral Rational = Integer
  proper :: Rational -> (Integer, Rational)
  proper (Ratio n d) =
    let (q, r) = euclidean n d
     in (q, Ratio r d)
instance Fractional Ration where
  type Integral Ration = Natural
  proper :: Ration -> (Natural, Ration)
  proper (Ratio n d) =
    let (q, r) = euclidean n d
     in (q, Ratio r d)
instance Fractional Float where
  type Integral Float = Integer
  proper :: Float -> (Integer, Float)
  proper = Num.properFraction
instance Fractional Double where
  type Integral Double = Integer
  proper :: Double -> (Integer, Double)
  proper = Num.properFraction
instance (HasResolution e) => Fractional (Fixed e) where
  type Integral (Fixed e) = Integer
  proper :: Fixed e -> (Integer, Fixed e)
  proper = Num.properFraction
instance
  ( Fractional x
  , From (Complex (Integral x)) (Complex x)
  , Addition (Integral (Complex x)) (Complex x) (Complex x)
  , Euclidean (Integral (Complex x))
  ) =>
  Fractional (Complex x)
  where
  type Integral (Complex x) = Complex (Integral x)
  proper :: Complex x -> (Complex (Integral x), Complex x)
  proper (a :+ bi) = case (proper a, proper bi) of
    ((ia, fa), (ib, fb)) -> (ia :+ ib, fa :+ fb)

-- Logarithmic

class Logarithmic x where
  exp :: x -> x
  log :: x -> x
  logBase :: x -> x -> x

instance Logarithmic Float where
  exp :: Float -> Float
  exp = Num.exp
  log :: Float -> Float
  log = Num.log
  logBase :: Float -> Float -> Float
  logBase = Num.logBase

instance Logarithmic Double where
  exp :: Double -> Double
  exp = Num.exp
  log :: Double -> Double
  log = Num.log
  logBase :: Double -> Double -> Double
  logBase = Num.logBase

-- Trigonometric

class Trigonometric x where
  pi :: x
  sin :: x -> x
  cos :: x -> x
  tan :: x -> x
  arcsin :: x -> x
  arccos :: x -> x
  arctan :: x -> x

instance Trigonometric Float where
  pi :: Float
  pi = Num.pi
  sin :: Float -> Float
  sin = Num.sin
  cos :: Float -> Float
  cos = Num.cos
  tan :: Float -> Float
  tan = Num.tan
  arcsin :: Float -> Float
  arcsin = Num.asin
  arccos :: Float -> Float
  arccos = Num.acos
  arctan :: Float -> Float
  arctan = Num.atan

instance Trigonometric Double where
  pi :: Double
  pi = Num.pi
  sin :: Double -> Double
  sin = Num.sin
  cos :: Double -> Double
  cos = Num.cos
  tan :: Double -> Double
  tan = Num.tan
  arcsin :: Double -> Double
  arcsin = Num.asin
  arccos :: Double -> Double
  arccos = Num.acos
  arctan :: Double -> Double
  arctan = Num.atan

-- Hyperbolic

class (Trigonometric x) => Hyperbolic x where
  sinh :: x -> x
  cosh :: x -> x
  tanh :: x -> x
  arcsinh :: x -> x
  arccosh :: x -> x
  arctanh :: x -> x

instance Hyperbolic Float where
  sinh :: Float -> Float
  sinh = Num.sinh
  cosh :: Float -> Float
  cosh = Num.cosh
  tanh :: Float -> Float
  tanh = Num.tanh
  arcsinh :: Float -> Float
  arcsinh = Num.asinh
  arccosh :: Float -> Float
  arccosh = Num.acosh
  arctanh :: Float -> Float
  arctanh = Num.atanh

instance Hyperbolic Double where
  sinh :: Double -> Double
  sinh = Num.sinh
  cosh :: Double -> Double
  cosh = Num.cosh
  tanh :: Double -> Double
  tanh = Num.tanh
  arcsinh :: Double -> Double
  arcsinh = Num.asinh
  arccosh :: Double -> Double
  arccosh = Num.acosh
  arctanh :: Double -> Double
  arctanh = Num.atanh
