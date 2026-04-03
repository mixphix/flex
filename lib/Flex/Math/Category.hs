{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{- HLINT ignore "Avoid lambda" -}
{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Use first" -}
{- HLINT ignore "Use unless" -}
{- HLINT ignore "Use when" -}
{- HLINT ignore "Use >=>" -}

module Flex.Math.Category
  ( Category (type Objects, id, (.))
  , C0
  , C2
  , CC
  , Composed
  --
  , Morphisms (morphism)
  , ($$)
  , Along
  --
  , type (~>) (OrdArrow, unOrdArrow)
  --
  , Ix (Ix, ix)
  , IxAlong
  , imorphism
  , OrdAlong
  --
  , Transform (Transform, transform)
  , type (-->)
  --
  , Folds (foldWith)
  , fold
  , foldl
  , foldlM
  , foldr
  , foldWithA
  , Foldable
  , traverse_
  , for_
  , IxFoldable
  , ifoldWith
  , ifoldl
  , ifoldr
  , ifoldWithA
  , itraverse_
  , ifor_
  , Folds1 (foldWith1)
  , Foldable1
  , fold1
  , foldl1'
  , foldr1
  , IxFoldable1
  , ifoldWith1
  , ifoldl1'
  , ifoldr1
  --
  , Against
  , Phantom
  , phantom
  --
  , Along2
  , morphism'
  , along2
  --
  , Traversals (traverse)
  , Traversable
  , sequence
  , for
  , mapAccumM
  , mapAccum
  , IxTraversable
  , itraverse
  , ifor
  , Traversals1 (traverse1)
  , Traversable1
  , sequence1
  , for1
  , IxTraversable1
  --
  , Pure (pure)
  , Apply ((<*>), liftA2)
  , liftA3
  , (<*)
  , (*>)
  , Applicative
  , Bind ((>>=))
  , (>>)
  , join
  , Monad
  , (>=>)
  , (=<<)
  , (<=<)
  , liftM2
  , liftM3
  , ap
  , void
  , when
  , unless
  --
  , Foldable2 (foldWith2)
  , Traversable2 (traverse2)
  --
  , Nil (nil)
  , guard
  , Alt ((<|>))
  , asum1
  , Alternative
  , asum
  , Option (Option, getOption)
  --
  , Filterable
  , justs
  , filter
  , IxFilterable
  , ijusts
  , ifilter
  --
  , Witherable (witherK)
  , wither
  , filterA
  , IxWitherable (iwitherK)
  , iwither
  , ifilterA
  --
  , Copure (copure)
  , Extend (duplicate, extend)
  , Coapply ((<@>))
  , Comonad
  , StoreT (StoreT, runStoreT)
  , Store
  , store
  , runStore
  , ComonadStore (position, peek, peeks, seek, seeks, experiment)
  , Collectable (collect, distribute)
  , cotraverse
  , Tabulation (type Table, fromTable, toTable)
  , Adjunction (unit, counit, left, right)
  , zipR
  , unzipR
  , cozipL
  , uncozipL
  , Cotabulation (type Cotable, fromCotable, toCotable)
  , Coadjunction (unitCo, counitCo, leftCo, rightCo)
  , phormism
  , Fletched
  , (#.)
  , (.#)
  , fletch
  , Forget (Forget, runForget)
  , Strong (product0, product1)
  , Costrong (unproduct0, unproduct1)
  , Choice (inl, inr)
  , Cochoice (outl, outr)
  , Sieve (sieve)
  , Cosieve (cosieve)
  , Representable (type Representation, represent)
  , Corepresentable (type Corepresentation, corepresent)
  , Closed (closed)
  , Conjoined (promap, conjoined)
  , Ixed (ixed)
  , (<.)
  , (.>)
  , withIndex
  , selfIndex
  , asIndex
  , reindexed
  , icompose
  , (<.>)
  , Procompose (Procompose)
    --
  , StateT (StateT, runStateT)
  , State
  , state
  , runState
  , get
  , gets
  , put
  , modify
  ) where

import Control.Applicative qualified as Control
import Control.Arrow (Kleisli (..), (&&&), (|||))
import Control.Category qualified as Control
import Control.Monad qualified as Control
import Control.Monad.Fix (MonadFix (..), fix)
import Data.Bool (Bool, not, otherwise)
import Data.Coerce (Coercible, coerce)
import Data.Complex (Complex (..), imagPart, realPart)
import Data.Either (Either (..), either)
import Data.Enum (Enum (..))
import Data.Foldable qualified as Data
import Data.Function (const, flip, ($))
import Data.Functor qualified as Data
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Contravariant (Op (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (..))
import Data.Functor.Sum (Sum (..))
import Data.Int (Int)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntMap.Internal qualified as IntMap
import Data.IntSet.Internal.IntTreeCommons qualified as Internal
import Data.Kind (Constraint, Type)
import Data.List (List)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as List1
import Data.List1 (List1, pattern Sole, pattern (:||))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Internal qualified as Map
import Data.Maybe (Maybe (..), maybe)
import Data.Monoid (Monoid (..))
import Data.Ord (Ord)
import Data.Proxy
import Data.Semigroup (Arg (..), Endo (..), Semigroup (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable qualified as Data
import Data.Tuple (curry, fst, snd, uncurry)
import Data.Type.Equality (type (~))
import Data.Vector qualified as Vector
import Data.Void (Void, absurd)
import GHC.Arr (Array)
import GHC.Arr qualified as Array
import GHC.Base (bindIO, returnIO)
import GHC.Exts (oneShot)
import GHC.Generics
import GHC.Num.Integer (Integer)
import GHC.Real (fromIntegral)
import Numeric.Natural (Natural)
import System.IO (IO)

-- Categories and Functors

type C0 :: k -> Constraint
class C0 x
instance C0 x

type C2 :: (k -> Constraint) -> (k -> Constraint) -> (k -> Constraint)
class (c0 x, c1 x) => C2 c0 c1 x
instance (c0 x, c1 x) => C2 c0 c1 x

type CC :: (k -> Constraint) -> (j -> k) -> (j -> Constraint)
class (c0 (f x)) => CC c0 f x
instance (c0 (f x)) => CC c0 f x

type Composed ::
  (j -> Constraint) -> (k -> Constraint) -> (j -> k) -> Constraint
class (forall x. ((g x) => c (f x))) => Composed g c f

class (Objects c x) => Objects' c x
instance (Objects c x) => Objects' c x

type Category :: (k -> k -> Type) -> Constraint
class Category cat where
  type Objects cat :: k -> Constraint
  id :: (Objects cat x) => cat x x
  (.) ::
    (Objects cat x, Objects cat y, Objects cat z) =>
    cat y z -> cat x y -> cat x z

instance Category (->) where
  type Objects (->) = C0
  id :: x -> x
  id x = x
  (.) :: (y -> z) -> (x -> y) -> x -> z
  y_z . x_y = \x -> y_z (x_y x)
instance Category Op where
  type Objects Op = C0
  id :: Op x x
  id = Op id
  (.) :: Op y z -> Op x y -> Op x z
  Op y_z . Op x_y = Op (x_y . y_z)
instance (Monad f) => Category (Kleisli f) where
  type Objects (Kleisli f) = C0
  id :: Kleisli f x x
  id = Kleisli pure
  (.) :: Kleisli f y z -> Kleisli f x y -> Kleisli f x z
  Kleisli y_fz . Kleisli x_fy = Kleisli (y_fz <=< x_fy)

data (~>) x y = (Ord x, Ord y) => OrdArrow {unOrdArrow :: x -> y}

instance Category (~>) where
  type Objects (~>) = Ord
  id :: (Ord x) => x ~> x
  id = OrdArrow id
  (.) :: (Ord x, Ord y, Ord z) => (y ~> z) -> (x ~> y) -> (x ~> z)
  OrdArrow y_z . OrdArrow x_y = OrdArrow (y_z . x_y)

type Morphisms ::
  (j -> j -> Type) ->
  (k -> k -> Type) ->
  (j -> k) ->
  Constraint
class (Category d) => Morphisms c d f where
  morphism :: c x y -> d (f x) (f y)

type Along f = Morphisms (->) (->) f

instance Morphisms (->) (->) Identity where
  morphism :: (x -> y) -> Identity x -> Identity y
  morphism x_y (Identity x) = Identity (x_y x)
instance Morphisms (->) (->) Maybe where
  morphism :: (x -> y) -> Maybe x -> Maybe y
  morphism x_y = \case
    Nothing -> Nothing
    Just x -> Just (x_y x)
instance Morphisms (->) (->) List1 where
  morphism :: (x -> y) -> List1 x -> List1 y
  morphism x_y = \case
    Sole x -> Sole (x_y x)
    x :|| xs -> x_y x :|| morphism x_y xs
instance Morphisms (->) (->) List where
  morphism :: (x -> y) -> List x -> List y
  morphism x_y = \case
    [] -> []
    x : xs -> x_y x : morphism x_y xs
instance Morphisms (->) (->) Complex where
  morphism :: (x -> y) -> Complex x -> Complex y
  morphism x_y (r :+ i) = x_y r :+ x_y i
instance Morphisms (->) (->) ((->) z) where
  morphism :: (x -> y) -> (z -> x) -> (z -> y)
  morphism = (.)
instance Morphisms (->) (->) (Either z) where
  morphism :: (x -> y) -> Either z x -> Either z y
  morphism x_y = \case
    Left z -> Left z
    Right x -> Right (x_y x)
instance Morphisms (->) (->) Proxy where
  morphism :: (x -> y) -> Proxy x -> Proxy y
  morphism _ Proxy = Proxy
instance Morphisms (->) (->) (Const z) where
  morphism :: (x -> y) -> Const z x -> Const z y
  morphism _ (Const z) = Const z
instance Morphisms (->) (->) (Arg z) where
  morphism :: (x -> y) -> Arg z x -> Arg z y
  morphism x_y (Arg z x) = Arg z (x_y x)
instance (Along f) => Morphisms (->) (->) (Kleisli f z) where
  morphism :: (x -> y) -> Kleisli f z x -> Kleisli f z y
  morphism x_y (Kleisli z_fx) = Kleisli (morphism x_y . z_fx)
instance (Along f, Along g) => Morphisms (->) (->) (Product f g) where
  morphism :: (x -> y) -> Product f g x -> Product f g y
  morphism x_y (Pair fx gx) = Pair (morphism x_y fx) (morphism x_y gx)
instance (Along f, Along g) => Morphisms (->) (->) (Sum f g) where
  morphism :: (x -> y) -> Sum f g x -> Sum f g y
  morphism x_y = \case
    InL fx -> InL (morphism x_y fx)
    InR gx -> InR (morphism x_y gx)
instance (Along f, Along g) => Morphisms (->) (->) (Compose f g) where
  morphism :: forall x y. (x -> y) -> Compose f g x -> Compose f g y
  morphism x_y (Compose fgx) =
    Compose (morphism (morphism x_y :: g x -> g y) fgx)
instance Morphisms (->) (->) ((,) a) where
  morphism :: (x -> y) -> (a, x) -> (a, y)
  morphism x_y (a, x) = (a, x_y x)
instance Morphisms (->) (->) ((,,) a b) where
  morphism :: (x -> y) -> (a, b, x) -> (a, b, y)
  morphism x_y (a, b, x) = (a, b, x_y x)
instance Morphisms (->) (->) ((,,,) a b c) where
  morphism :: (x -> y) -> (a, b, c, x) -> (a, b, c, y)
  morphism x_y (a, b, c, x) = (a, b, c, x_y x)
instance Morphisms (->) (->) ((,,,,) a b c d) where
  morphism :: (x -> y) -> (a, b, c, d, x) -> (a, b, c, d, y)
  morphism x_y (a, b, c, d, x) = (a, b, c, d, x_y x)
instance Morphisms (->) (->) ((,,,,,) a b c d e) where
  morphism :: (x -> y) -> (a, b, c, d, e, x) -> (a, b, c, d, e, y)
  morphism x_y (a, b, c, d, e, x) = (a, b, c, d, e, x_y x)
instance Morphisms (->) (->) ((,,,,,,) a b c d e f) where
  morphism ::
    (x -> y) -> (a, b, c, d, e, f, x) -> (a, b, c, d, e, f, y)
  morphism x_y (a, b, c, d, e, f, x) = (a, b, c, d, e, f, x_y x)
instance Morphisms (->) (->) ((,,,,,,,) a b c d e f g) where
  morphism ::
    (x -> y) -> (a, b, c, d, e, f, g, x) -> (a, b, c, d, e, f, g, y)
  morphism x_y (a, b, c, d, e, f, g, x) = (a, b, c, d, e, f, g, x_y x)
instance Morphisms (->) (->) IO where
  morphism :: (x -> y) -> IO x -> IO y
  morphism = Data.fmap

instance Morphisms (->) (->) V1 where
  morphism :: (x -> y) -> V1 x -> V1 y
  morphism _ v = case v of {}
instance Morphisms (->) (->) U1 where
  morphism :: (x -> y) -> U1 x -> U1 y
  morphism _ U1 = U1
instance Morphisms (->) (->) Par1 where
  morphism :: (x -> y) -> Par1 x -> Par1 y
  morphism x_y (Par1 x) = Par1 (x_y x)
instance (Along f) => Morphisms (->) (->) (Rec1 f) where
  morphism :: (x -> y) -> Rec1 f x -> Rec1 f y
  morphism x_y (Rec1 fx) = Rec1 (morphism x_y fx)
instance Morphisms (->) (->) (K1 i c) where
  morphism :: (x -> y) -> K1 i c x -> K1 i c y
  morphism _ (K1 c) = K1 c
instance (Along f) => Morphisms (->) (->) (M1 i c f) where
  morphism :: (x -> y) -> M1 i c f x -> M1 i c f y
  morphism x_y (M1 fx) = M1 (morphism x_y fx)
instance (Along f, Along g) => Morphisms (->) (->) (f :*: g) where
  morphism :: (x -> y) -> (f :*: g) x -> (f :*: g) y
  morphism x_y (fx :*: gx) =
    morphism x_y fx :*: morphism x_y gx
instance (Along f, Along g) => Morphisms (->) (->) (f :+: g) where
  morphism :: (x -> y) -> (f :+: g) x -> (f :+: g) y
  morphism x_y = \case
    L1 fx -> L1 (morphism x_y fx)
    R1 gx -> R1 (morphism x_y gx)
instance (Along f, Along g) => Morphisms (->) (->) (f :.: g) where
  morphism :: forall x y. (x -> y) -> (f :.: g) x -> (f :.: g) y
  morphism x_y (Comp1 fgx) =
    Comp1 (morphism (morphism x_y :: g x -> g y) fgx)

instance Morphisms (->) (->) (Array i) where
  morphism :: (x -> y) -> Array i x -> Array i y
  morphism = Data.fmap
instance Morphisms (->) (->) (Map k) where
  morphism :: (x -> y) -> Map k x -> Map k y
  morphism = Data.fmap
instance Morphisms (->) (->) IntMap where
  morphism :: (x -> y) -> IntMap x -> IntMap y
  morphism = Data.fmap
instance Morphisms (->) (->) Seq where
  morphism :: (x -> y) -> Seq x -> Seq y
  morphism = Data.fmap

instance Morphisms (->) (->) Vector.Vector where
  morphism :: (x -> y) -> Vector.Vector x -> Vector.Vector y
  morphism = Data.fmap

type OrdAlong f = Morphisms (~>) (->) f

instance Morphisms (~>) (->) Set where
  morphism :: (x ~> y) -> Set x -> Set y
  morphism (OrdArrow x_y) = Set.map x_y

($$) :: (Along f) => f (x -> z) -> x -> f z
fxz $$ x = morphism ($ x) fxz

type Ix :: Type -> Type -> Type -> Type
newtype Ix i x y = Ix {ix :: i -> x -> y}

instance Data.Functor (Ix i z) where
  fmap :: (x -> y) -> Ix i z x -> Ix i z y
  fmap x_y (Ix i_z_y) = Ix \i -> x_y . i_z_y i
instance Control.Applicative (Ix i z) where
  pure :: x -> Ix i z x
  pure x = Ix (const (const x))
  (<*>) :: Ix i z (x -> y) -> Ix i z x -> Ix i z y
  Ix i_z_x_y <*> Ix i_z_x =
    Ix \i z -> i_z_x_y i z (i_z_x i z)
instance Control.Monad (Ix i z) where
  (>>=) :: Ix i z a -> (a -> Ix i z b) -> Ix i z b
  Ix i_z_x >>= x_Iizy =
    Ix \i z -> ix (x_Iizy (i_z_x i z)) i z
instance Control.Category (Ix i) where
  id :: Ix i x x
  id = Ix (const id)
  (.) :: Ix i y z -> Ix i x y -> Ix i x z
  Ix i_y_z . Ix i_x_y = Ix \i -> i_y_z i . i_x_y i
instance MonadFix (Ix i z) where
  mfix :: (x -> Ix i z x) -> Ix i z x
  mfix f = Ix \i z -> let o = ix (f o) i z in o
instance Category (Ix i) where
  type Objects (Ix i) = C0
  id :: Ix i x x
  id = Ix (const id)
  (.) :: Ix i y z -> Ix i x y -> Ix i x z
  Ix i_y_z . Ix i_x_y = Ix (liftA2 (.) i_y_z i_x_y)
instance Morphisms (->) (->) (Ix i z) where
  morphism :: (x -> y) -> Ix i z x -> Ix i z y
  morphism x_y (Ix i_z_y) = Ix \i -> x_y . i_z_y i
instance Pure (Ix i z) where
  pure :: x -> Ix i z x
  pure x = Ix (const (const x))
instance Apply (Ix i z) where
  (<*>) :: Ix i z (x -> y) -> Ix i z x -> Ix i z y
  Ix i_z_x_y <*> Ix i_z_x =
    Ix \i z -> i_z_x_y i z (i_z_x i z)
instance Bind (Ix i z) where
  (>>=) :: Ix i z a -> (a -> Ix i z b) -> Ix i z b
  Ix i_z_x >>= x_Iizy =
    Ix \i z -> ix (x_Iizy (i_z_x i z)) i z

type IxAlong i f = Morphisms (Ix i) (->) f

imorphism :: (IxAlong i f) => (i -> x -> y) -> f x -> f y
imorphism = morphism . Ix

instance Morphisms (Ix ()) (->) Identity where
  morphism :: Ix () x y -> Identity x -> Identity y
  morphism (Ix u_x_y) (Identity x) = Identity (u_x_y () x)
instance Morphisms (Ix ()) (->) Maybe where
  morphism :: Ix () x y -> Maybe x -> Maybe y
  morphism (Ix u_x_y) = \case
    Nothing -> Nothing
    Just x -> Just (u_x_y () x)
instance Morphisms (Ix z) (->) ((,) z) where
  morphism :: Ix z x y -> (z, x) -> (z, y)
  morphism (Ix z_x_y) (z, x) = (z, z_x_y z x)
instance Morphisms (Ix i) (->) ((->) i) where
  morphism :: Ix i x y -> (i -> x) -> (i -> y)
  morphism (Ix i_x_y) i_x = i_x_y <*> i_x
instance Morphisms (Ix i) (->) (Ix i i) where
  morphism :: Ix i x y -> Ix i i x -> Ix i i y
  morphism (Ix i_x_y) (Ix i_i'_x) = Ix \i -> i_x_y i . i_i'_x i
instance Morphisms (Ix ()) (->) (Either y) where
  morphism :: Ix () x z -> Either y x -> Either y z
  morphism (Ix u_x_z) = \case
    Left y -> Left y
    Right x -> Right (u_x_z () x)
instance Morphisms (Ix (Either () ())) (->) Complex where
  morphism ::
    Ix (Either () ()) x y -> Complex x -> Complex y
  morphism (Ix e_x_z) (xr :+ xi) =
    e_x_z (Left ()) xr :+ e_x_z (Right ()) xi
instance Morphisms (Ix Void) (->) Proxy where
  morphism :: Ix Void x y -> Proxy x -> Proxy y
  morphism (Ix _) Proxy = Proxy
instance Morphisms (Ix Void) (->) (Const z) where
  morphism :: Ix Void x y -> Const z x -> Const z y
  morphism (Ix _) (Const z) = Const z
instance Morphisms (Ix z) (->) (Arg z) where
  morphism :: Ix z x y -> Arg z x -> Arg z y
  morphism (Ix z_x_z) (Arg z x) = Arg z (z_x_z z x)
instance
  (IxAlong i f, IxAlong j g) =>
  Morphisms (Ix (Either i j)) (->) (Product f g)
  where
  morphism ::
    Ix (Either i j) x y -> Product f g x -> Product f g y
  morphism (Ix ij_x_y) (Pair fx gx) =
    Pair
      (morphism (Ix (ij_x_y . Left)) fx)
      (morphism (Ix (ij_x_y . Right)) gx)
instance
  (IxAlong i f, IxAlong j g) =>
  Morphisms (Ix (Either i j)) (->) (Sum f g)
  where
  morphism ::
    Ix (Either i j) x y -> Sum f g x -> Sum f g y
  morphism (Ix ij_x_y) = \case
    InL fx -> InL (morphism (Ix (ij_x_y . Left)) fx)
    InR gx -> InR (morphism (Ix (ij_x_y . Right)) gx)
instance
  (IxAlong i f, IxAlong j g) =>
  Morphisms (Ix (i, j)) (->) (Compose f g)
  where
  morphism ::
    Ix (i, j) x y -> Compose f g x -> Compose f g y
  morphism (Ix ij_x_y) (Compose fgx) = Compose do
    morphism
      (Ix \i -> morphism (Ix \j -> ij_x_y (i, j)))
      fgx

instance Morphisms (Ix Int) (->) List where
  morphism :: Ix Int x y -> List x -> List y
  morphism (Ix i_x_z) = flip fix 0 \rec i -> \case
    [] -> []
    x : xs -> i_x_z i x : rec (succ i) xs
instance Morphisms (Ix Integer) (->) List where
  morphism :: Ix Integer x y -> List x -> List y
  morphism (Ix i_x_z) = flip fix 0 \rec i -> \case
    [] -> []
    x : xs -> i_x_z i x : rec (succ i) xs
instance Morphisms (Ix Natural) (->) List where
  morphism :: Ix Natural x y -> List x -> List y
  morphism (Ix i_x_z) = flip fix 0 \rec i -> \case
    [] -> []
    x : xs -> i_x_z i x : rec (succ i) xs
instance Morphisms (Ix Int) (->) List1 where
  morphism :: Ix Int x y -> List1 x -> List1 y
  morphism (Ix i_x_z) = flip fix 0 \rec i -> \case
    Sole x -> Sole (i_x_z i x)
    x :|| xs -> i_x_z i x :|| rec (succ i) xs
instance Morphisms (Ix Integer) (->) List1 where
  morphism :: Ix Integer x y -> List1 x -> List1 y
  morphism (Ix i_x_z) = flip fix 0 \rec i -> \case
    Sole x -> Sole (i_x_z i x)
    x :|| xs -> i_x_z i x :|| rec (succ i) xs
instance Morphisms (Ix Natural) (->) List1 where
  morphism :: Ix Natural x y -> List1 x -> List1 y
  morphism (Ix i_x_z) = flip fix 0 \rec i -> \case
    Sole x -> Sole (i_x_z i x)
    x :|| xs -> i_x_z i x :|| rec (succ i) xs

instance (Array.Ix i) => Morphisms (Ix i) (->) (Array i) where
  morphism :: Ix i e e' -> Array i e -> Array i e'
  morphism (Ix i_e_e') arr =
    Array.listArray
      (Array.bounds arr)
      (morphism (uncurry i_e_e') (Array.assocs arr))
instance Morphisms (Ix k) (->) (Map k) where
  morphism :: Ix k v v' -> Map k v -> Map k v'
  morphism (Ix k_v_v') = Map.mapWithKey k_v_v'
instance Morphisms (Ix Int) (->) IntMap where
  morphism :: Ix Int v v' -> IntMap v -> IntMap v'
  morphism (Ix i_v_v') = IntMap.mapWithKey i_v_v'
instance Morphisms (Ix Int) (->) Seq where
  morphism :: Ix Int x y -> Seq x -> Seq y
  morphism (Ix i_x_y) = Seq.mapWithIndex i_x_y
instance Morphisms (Ix Integer) (->) Seq where
  morphism :: Ix Integer x y -> Seq x -> Seq y
  morphism (Ix i_x_y) = Seq.mapWithIndex (i_x_y . fromIntegral)
instance Morphisms (Ix Natural) (->) Seq where
  morphism :: Ix Natural x y -> Seq x -> Seq y
  morphism (Ix i_x_y) = Seq.mapWithIndex (i_x_y . fromIntegral)
instance Morphisms (Ix Int) (->) Vector.Vector where
  morphism :: Ix Int x y -> Vector.Vector x -> Vector.Vector y
  morphism (Ix i_x_y) = Vector.imap i_x_y
instance Morphisms (Ix Integer) (->) Vector.Vector where
  morphism :: Ix Integer x y -> Vector.Vector x -> Vector.Vector y
  morphism (Ix i_x_y) = Vector.imap (i_x_y . fromIntegral)
instance Morphisms (Ix Natural) (->) Vector.Vector where
  morphism :: Ix Natural x y -> Vector.Vector x -> Vector.Vector y
  morphism (Ix i_x_y) = Vector.imap (i_x_y . fromIntegral)

instance Morphisms (Ix Void) (->) V1 where
  morphism :: Ix Void x y -> V1 x -> V1 y
  morphism _ v = case v of {}
instance Morphisms (Ix Void) (->) U1 where
  morphism :: Ix Void x y -> U1 x -> U1 y
  morphism (Ix _) U1 = U1
instance Morphisms (Ix ()) (->) Par1 where
  morphism :: Ix () x y -> Par1 x -> Par1 y
  morphism (Ix u_x_y) (Par1 x) = Par1 (u_x_y () x)
instance
  (IxAlong i f) =>
  Morphisms (Ix i) (->) (Rec1 f)
  where
  morphism :: Ix i x y -> Rec1 f x -> Rec1 f y
  morphism (Ix i_x_y) (Rec1 fx) =
    Rec1 (morphism (Ix i_x_y) fx)
instance Morphisms (Ix Void) (->) (K1 i c) where
  morphism :: Ix Void x y -> K1 i c x -> K1 i c y
  morphism _ (K1 c) = K1 c
instance
  (IxAlong j f) =>
  Morphisms (Ix j) (->) (M1 i c f)
  where
  morphism :: Ix j x y -> M1 i c f x -> M1 i c f y
  morphism (Ix j_x_y) (M1 fx) =
    M1 (morphism (Ix j_x_y) fx)
instance
  (IxAlong i f, IxAlong j g) =>
  Morphisms (Ix (Either i j)) (->) (f :*: g)
  where
  morphism ::
    Ix (Either i j) x y -> (f :*: g) x -> (f :*: g) y
  morphism (Ix ij_x_y) (fx :*: gx) =
    morphism (Ix (ij_x_y . Left)) fx
      :*: morphism (Ix (ij_x_y . Right)) gx
instance
  (IxAlong i f, IxAlong j g) =>
  Morphisms (Ix (Either i j)) (->) (f :+: g)
  where
  morphism ::
    Ix (Either i j) x y -> (f :+: g) x -> (f :+: g) y
  morphism (Ix ij_x_y) = \case
    L1 fx -> L1 (morphism (Ix (ij_x_y . Left)) fx)
    R1 gx -> R1 (morphism (Ix (ij_x_y . Right)) gx)
instance
  (IxAlong i f, IxAlong j g) =>
  Morphisms (Ix (i, j)) (->) (f :.: g)
  where
  morphism :: Ix (i, j) x y -> (f :.: g) x -> (f :.: g) y
  morphism (Ix ij_x_y) (Comp1 fgx) = Comp1 do
    morphism
      (Ix \i -> morphism (Ix \j -> ij_x_y (i, j)))
      fgx

type Folds ::
  (Type -> Type -> Type) ->
  (Type -> Type -> Type) ->
  (Type -> Type) ->
  Constraint
class (Category d) => Folds c d f where
  foldWith :: (Monoid z) => c x z -> d (f x) z

type Foldable f = Folds (->) (->) f

fold :: (Foldable f, Monoid x) => f x -> x
fold = foldWith (id :: x -> x)

foldl :: (Foldable f) => (y -> x -> y) -> y -> f x -> y
foldl y_x_y y0 fx =
  foldr
    (\x y_y -> oneShot \ !y -> y_y (y_x_y y x))
    id
    fx
    y0
foldlM :: forall m f x y. (Foldable f, Monad m) => (y -> x -> m y) -> y -> f x -> m y
foldlM y_x_my y0 fx = foldr c pure fx y0
 where
  c :: x -> (y -> m y) -> y -> m y
  c x k y = y_x_my y x >>= k
foldr :: (Foldable f) => (x -> y -> y) -> y -> f x -> y
foldr x_y_y y fx = foldWith (Endo . x_y_y) fx `appEndo` y

foldWithA :: (Foldable t, Applicative f, Monoid m) => (x -> f m) -> t x -> f m
foldWithA f = foldl (\ !fm x -> liftA2 (<>) fm (f x)) (pure mempty)

traverse_ ::
  (Foldable t, Applicative f) => (x -> f y) -> t x -> f ()
traverse_ x_fy = foldr (liftA2 (\ !_ () -> ()) . x_fy) (pure ())

for_ :: (Foldable t, Applicative f) => t x -> (x -> f y) -> f ()
for_ = flip traverse_

instance Folds (->) (->) Identity where
  foldWith :: (Monoid z) => (x -> z) -> Identity x -> z
  foldWith x_z (Identity x) = x_z x
instance Folds (->) (->) Maybe where
  foldWith :: (Monoid z) => (x -> z) -> Maybe x -> z
  foldWith x_z = \case
    Nothing -> mempty
    Just x -> x_z x
instance Folds (->) (->) List1 where
  foldWith :: (Monoid z) => (x -> z) -> List1 x -> z
  foldWith x_z = \case
    Sole x -> x_z x
    x :|| xs -> x_z x <> foldWith x_z xs
instance Folds (->) (->) List where
  foldWith :: (Monoid z) => (x -> z) -> List x -> z
  foldWith x_z = \case
    [] -> mempty
    x : xs -> x_z x <> foldWith x_z xs
instance Folds (->) (->) ((,) y) where
  foldWith :: (Monoid z) => (x -> z) -> (y, x) -> z
  foldWith x_z (_, x) = x_z x
instance Folds (->) (->) Complex where
  foldWith :: (Monoid z) => (x -> z) -> Complex x -> z
  foldWith x_z (xr :+ xi) = x_z xr <> x_z xi
instance Folds (->) (->) (Either y) where
  foldWith :: (Monoid z) => (x -> z) -> Either y x -> z
  foldWith x_z = \case
    Left _ -> mempty
    Right x -> x_z x
instance Folds (->) (->) Proxy where
  foldWith :: (Monoid z) => (x -> z) -> Proxy x -> z
  foldWith _ Proxy = mempty
instance Folds (->) (->) (Const y) where
  foldWith :: (Monoid z) => (x -> z) -> Const y x -> z
  foldWith _ (Const _) = mempty
instance Folds (->) (->) (Arg y) where
  foldWith :: (Monoid z) => (x -> z) -> Arg y x -> z
  foldWith x_z (Arg _ x) = x_z x
instance
  (Foldable f, Foldable g) =>
  Folds (->) (->) (Product f g)
  where
  foldWith :: (Monoid z) => (x -> z) -> Product f g x -> z
  foldWith x_z (Pair fx gx) = foldWith x_z fx <> foldWith x_z gx
instance
  (Foldable f, Foldable g) =>
  Folds (->) (->) (Sum f g)
  where
  foldWith :: (Monoid z) => (x -> z) -> Sum f g x -> z
  foldWith x_z = \case
    InL fx -> foldWith x_z fx
    InR gx -> foldWith x_z gx
instance
  (Foldable f, Foldable g) =>
  Folds (->) (->) (Compose f g)
  where
  foldWith ::
    forall x z.
    (Monoid z) => (x -> z) -> Compose f g x -> z
  foldWith x_z (Compose fgx) =
    foldWith (foldWith x_z :: g x -> z) fgx

instance Folds (->) (->) V1 where
  foldWith :: (Monoid z) => (x -> z) -> V1 x -> z
  foldWith _ _ = mempty
instance Folds (->) (->) U1 where
  foldWith :: (Monoid z) => (x -> z) -> U1 x -> z
  foldWith _ U1 = mempty
instance Folds (->) (->) Par1 where
  foldWith :: (Monoid z) => (x -> z) -> Par1 x -> z
  foldWith x_z (Par1 x) = x_z x
instance (Foldable f) => Folds (->) (->) (Rec1 f) where
  foldWith :: (Monoid z) => (x -> z) -> Rec1 f x -> z
  foldWith x_z (Rec1 fx) = foldWith x_z fx
instance Folds (->) (->) (K1 i c) where
  foldWith :: (Monoid z) => (x -> z) -> K1 i c x -> z
  foldWith _ (K1 _) = mempty
instance (Foldable f) => Folds (->) (->) (M1 i c f) where
  foldWith :: (Monoid z) => (x -> z) -> M1 i c f x -> z
  foldWith x_z (M1 fx) = foldWith x_z fx
instance
  (Foldable f, Foldable g) =>
  Folds (->) (->) (f :*: g)
  where
  foldWith :: (Monoid z) => (x -> z) -> (f :*: g) x -> z
  foldWith x_z (fx :*: gx) = foldWith x_z fx <> foldWith x_z gx
instance
  (Foldable f, Foldable g) =>
  Folds (->) (->) (f :+: g)
  where
  foldWith :: (Monoid z) => (x -> z) -> (f :+: g) x -> z
  foldWith x_z = \case
    L1 fx -> foldWith x_z fx
    R1 gx -> foldWith x_z gx
instance
  (Foldable f, Foldable g) =>
  Folds (->) (->) (f :.: g)
  where
  foldWith ::
    forall x z.
    (Monoid z) => (x -> z) -> (f :.: g) x -> z
  foldWith x_z (Comp1 fgx) =
    foldWith (foldWith x_z :: g x -> z) fgx

instance Folds (->) (->) (Array i) where
  foldWith :: (Monoid z) => (x -> z) -> Array i x -> z
  foldWith = Data.foldMap
instance Folds (->) (->) (Map k) where
  foldWith :: (Monoid z) => (v -> z) -> Map k v -> z
  foldWith = Data.foldMap
instance Folds (->) (->) IntMap where
  foldWith :: (Monoid z) => (v -> z) -> IntMap v -> z
  foldWith = Data.foldMap
instance Folds (->) (->) Set where
  foldWith :: (Monoid z) => (x -> z) -> Set x -> z
  foldWith = Data.foldMap
instance Folds (->) (->) Seq where
  foldWith :: (Monoid z) => (x -> z) -> Seq x -> z
  foldWith = Data.foldMap

instance Folds (->) (->) Vector.Vector where
  foldWith :: (Monoid z) => (x -> z) -> Vector.Vector x -> z
  foldWith = Data.foldMap

type IxFoldable i f = (Folds (->) (->) f, Folds (Ix i) (->) f)

ifoldWith :: (IxFoldable i f, Monoid z) => (i -> x -> z) -> f x -> z
ifoldWith = foldWith . Ix

ifoldl :: (IxFoldable i f) => (i -> y -> x -> y) -> y -> f x -> y
ifoldl i_y_x_y y0 fx =
  ifoldr
    (\i x y_y -> oneShot \ !y -> y_y (i_y_x_y i y x))
    id
    fx
    y0
ifoldr :: (IxFoldable i f) => (i -> x -> y -> y) -> y -> f x -> y
ifoldr i_x_y_y y fx =
  ifoldWith (\i -> Endo . i_x_y_y i) fx `appEndo` y

ifoldWithA :: (IxFoldable i t, Applicative f, Monoid m) => (i -> x -> f m) -> t x -> f m
ifoldWithA i_x_fm = ifoldl (\ i !fm x -> liftA2 (<>) fm (i_x_fm i x)) (pure mempty)

itraverse_ :: (IxFoldable i f, Applicative g) => (i -> x -> g y) -> f x -> g ()
itraverse_ i_x_gy = ifoldr f (pure ())
 where
  f i x k = i_x_gy i x *> k

ifor_ :: (IxFoldable i f, Applicative g) => f x -> (i -> x -> g y) -> g ()
ifor_ = flip itraverse_

instance Folds (Ix ()) (->) Identity where
  foldWith :: (Monoid z) => Ix () x z -> Identity x -> z
  foldWith (Ix u_x_z) (Identity x) = u_x_z () x
instance Folds (Ix ()) (->) Maybe where
  foldWith :: (Monoid z) => Ix () x z -> Maybe x -> z
  foldWith (Ix u_x_z) = \case
    Nothing -> mempty
    Just x -> u_x_z () x
instance Folds (Ix y) (->) ((,) y) where
  foldWith :: (Monoid z) => Ix y x z -> (y, x) -> z
  foldWith (Ix z_x_gy) (z, x) = z_x_gy z x
instance Folds (Ix (Either () ())) (->) Complex where
  foldWith ::
    (Monoid z) =>
    Ix (Either () ()) x z -> Complex x -> z
  foldWith (Ix e_x_z) (xr :+ xi) =
    e_x_z (Left ()) xr <> e_x_z (Right ()) xi
instance Folds (Ix ()) (->) (Either y) where
  foldWith :: (Monoid z) => Ix () x z -> Either y x -> z
  foldWith (Ix u_x_z) = \case
    Left _ -> mempty
    Right x -> u_x_z () x
instance Folds (Ix Void) (->) Proxy where
  foldWith :: (Monoid z) => Ix Void x z -> Proxy x -> z
  foldWith _ Proxy = mempty
instance Folds (Ix Void) (->) (Const y) where
  foldWith :: (Monoid z) => Ix Void x z -> Const y x -> z
  foldWith (Ix _) (Const _) = mempty
instance Folds (Ix y) (->) (Arg y) where
  foldWith :: (Monoid z) => Ix y x z -> Arg y x -> z
  foldWith (Ix y_x_z) (Arg y x) = y_x_z y x
instance
  (IxFoldable i f, IxFoldable j g) =>
  Folds (Ix (Either i j)) (->) (Product f g)
  where
  foldWith ::
    (Monoid z) => Ix (Either i j) x z -> (Product f g) x -> z
  foldWith (Ix ij_x_z) (Pair fx gx) =
    foldWith (Ix (ij_x_z . Left)) fx
      <> foldWith (Ix (ij_x_z . Right)) gx
instance
  (IxFoldable i f, IxFoldable j g) =>
  Folds (Ix (Either i j)) (->) (Sum f g)
  where
  foldWith ::
    (Monoid z) => Ix (Either i j) x z -> (Sum f g) x -> z
  foldWith (Ix ij_x_z) = \case
    InL fx -> foldWith (Ix (ij_x_z . Left)) fx
    InR gx -> foldWith (Ix (ij_x_z . Right)) gx
instance
  (IxFoldable i f, IxFoldable j g) =>
  Folds (Ix (i, j)) (->) (Compose f g)
  where
  foldWith :: (Monoid z) => Ix (i, j) x z -> (Compose f g) x -> z
  foldWith (Ix ij_x_z) (Compose fgx) =
    foldWith (Ix \i -> foldWith (Ix \j -> ij_x_z (i, j))) fgx
instance Folds (Ix Int) (->) List1 where
  foldWith :: (Monoid z) => Ix Int x z -> List1 x -> z
  foldWith (Ix i_x_z) = flip fix 0 \rec i -> \case
    Sole x -> i_x_z i x
    x :|| xs -> i_x_z i x <> rec (succ i) xs
instance Folds (Ix Integer) (->) List1 where
  foldWith :: (Monoid z) => Ix Integer x z -> List1 x -> z
  foldWith (Ix i_x_z) = flip fix 0 \rec i -> \case
    Sole x -> i_x_z i x
    x :|| xs -> i_x_z i x <> rec (succ i) xs
instance Folds (Ix Natural) (->) List1 where
  foldWith :: (Monoid z) => Ix Natural x z -> List1 x -> z
  foldWith (Ix i_x_z) = flip fix 0 \rec i -> \case
    Sole x -> i_x_z i x
    x :|| xs -> i_x_z i x <> rec (succ i) xs
instance Folds (Ix Integer) (->) List where
  foldWith :: (Monoid z) => Ix Integer x z -> List x -> z
  foldWith (Ix i_x_z) = flip fix 0 \rec i -> \case
    [] -> mempty
    x : xs -> i_x_z i x <> rec (succ i) xs
instance Folds (Ix Int) (->) List where
  foldWith :: (Monoid z) => Ix Int x z -> List x -> z
  foldWith (Ix i_x_z) = flip fix 0 \rec i -> \case
    [] -> mempty
    x : xs -> i_x_z i x <> rec (succ i) xs
instance Folds (Ix Natural) (->) List where
  foldWith :: (Monoid z) => Ix Natural x z -> List x -> z
  foldWith (Ix i_x_z) = flip fix 0 \rec i -> \case
    [] -> mempty
    x : xs -> i_x_z i x <> rec (succ i) xs

instance (Array.Ix i) => Folds (Ix i) (->) (Array i) where
  foldWith :: (Monoid z) => Ix i x z -> Array i x -> z
  foldWith (Ix i_e_e') =
    foldWith (uncurry i_e_e') . Array.assocs
instance Folds (Ix k) (->) (Map k) where
  foldWith :: (Monoid z) => Ix k x z -> Map k x -> z
  foldWith (Ix k_v_z) = Map.foldMapWithKey k_v_z
instance Folds (Ix Int) (->) IntMap where
  foldWith :: (Monoid z) => Ix Int v z -> IntMap v -> z
  foldWith (Ix i_v_z) = IntMap.foldMapWithKey i_v_z
instance Folds (Ix Int) (->) Seq where
  foldWith :: (Monoid z) => Ix Int x z -> Seq x -> z
  foldWith (Ix i_x_z) = Seq.foldMapWithIndex i_x_z
instance Folds (Ix Integer) (->) Seq where
  foldWith :: (Monoid z) => Ix Integer x z -> Seq x -> z
  foldWith (Ix i_x_z) = Seq.foldMapWithIndex (i_x_z . fromIntegral)
instance Folds (Ix Natural) (->) Seq where
  foldWith :: (Monoid z) => Ix Natural x z -> Seq x -> z
  foldWith (Ix i_x_z) = Seq.foldMapWithIndex (i_x_z . fromIntegral)

instance Folds (Ix Int) (->) Vector.Vector where
  foldWith :: (Monoid z) => Ix Int x z -> Vector.Vector x -> z
  foldWith (Ix i_x_z) = Vector.foldMap (uncurry i_x_z) . Vector.indexed
instance Folds (Ix Integer) (->) Vector.Vector where
  foldWith :: (Monoid z) => Ix Integer x z -> Vector.Vector x -> z
  foldWith (Ix i_x_z) = Vector.foldMap (uncurry (i_x_z . fromIntegral)) . Vector.indexed
instance Folds (Ix Natural) (->) Vector.Vector where
  foldWith :: (Monoid z) => Ix Natural x z -> Vector.Vector x -> z
  foldWith (Ix i_x_z) = Vector.foldMap (uncurry (i_x_z . fromIntegral)) . Vector.indexed

instance Folds (Ix Void) (->) V1 where
  foldWith :: (Monoid z) => Ix Void x z -> V1 x -> z
  foldWith (Ix _) v = case v of {}
instance Folds (Ix Void) (->) U1 where
  foldWith :: (Monoid z) => Ix Void x z -> U1 x -> z
  foldWith (Ix _) U1 = mempty
instance Folds (Ix ()) (->) Par1 where
  foldWith :: (Monoid z) => Ix () x z -> Par1 x -> z
  foldWith (Ix u_x_z) (Par1 x) = u_x_z () x
instance (IxFoldable i f) => Folds (Ix i) (->) (Rec1 f) where
  foldWith :: (Monoid z) => Ix i x z -> Rec1 f x -> z
  foldWith (Ix i_x_z) (Rec1 fx) = foldWith (Ix i_x_z) fx
instance Folds (Ix Void) (->) (K1 i c) where
  foldWith :: (Monoid z) => Ix Void x z -> K1 i c x -> z
  foldWith (Ix _) (K1 _) = mempty
instance (IxFoldable j f) => Folds (Ix j) (->) (M1 i c f) where
  foldWith :: (Monoid z) => Ix j x z -> M1 i c f x -> z
  foldWith (Ix i_x_z) (M1 fx) = foldWith (Ix i_x_z) fx
instance
  (IxFoldable i f, IxFoldable j g) =>
  Folds (Ix (Either i j)) (->) (f :*: g)
  where
  foldWith ::
    (Monoid z) => Ix (Either i j) x z -> (f :*: g) x -> z
  foldWith (Ix ij_x_z) (fx :*: gx) =
    foldWith (Ix (ij_x_z . Left)) fx
      <> foldWith (Ix (ij_x_z . Right)) gx
instance
  (IxFoldable i f, IxFoldable j g) =>
  Folds (Ix (Either i j)) (->) (f :+: g)
  where
  foldWith ::
    (Monoid z) => Ix (Either i j) x z -> (f :+: g) x -> z
  foldWith (Ix ij_x_z) = \case
    L1 fx -> foldWith (Ix (ij_x_z . Left)) fx
    R1 gx -> foldWith (Ix (ij_x_z . Right)) gx
instance
  (IxFoldable i f, IxFoldable j g) =>
  Folds (Ix (i, j)) (->) (f :.: g)
  where
  foldWith :: (Monoid z) => Ix (i, j) x z -> (f :.: g) x -> z
  foldWith (Ix ij_x_z) (Comp1 fgx) =
    foldWith (Ix \i -> foldWith (Ix \j -> ij_x_z (i, j))) fgx

newtype FromMaybe x = FromMaybe {appFromMaybe :: Maybe x -> x}
instance Semigroup (FromMaybe x) where
  (<>) :: FromMaybe x -> FromMaybe x -> FromMaybe x
  FromMaybe mx_x <> FromMaybe my_y = FromMaybe (mx_x . Just . my_y)

class Folds1 c d f where
  foldWith1 :: (Semigroup z) => c x z -> d (f x) z

type Foldable1 f = Folds1 (->) (->) f

fold1 :: forall f x. (Foldable1 f, Semigroup x) => f x -> x
fold1 = foldWith1 (id :: x -> x)

foldrWith1 :: (Foldable1 f) => (x -> y) -> (x -> y -> y) -> f x -> y
foldrWith1 x_y x_y_y fx =
  appFromMaybe (foldWith1 (FromMaybe . h) fx) Nothing
 where
  h x Nothing = x_y x
  h x (Just y) = x_y_y x y

foldlWith1' :: (Foldable1 f) => (x -> y) -> (y -> x -> y) -> f x -> y
foldlWith1' x_y y_x_y fx = foldrWith1 f' g' fx Nothing
 where
  f' x Nothing = x_y x
  f' x (Just !y) = y_x_y y x
  g' x f Nothing = f (Just (x_y x))
  g' x f (Just !y) = f (Just (y_x_y y x))

foldl1' :: (Foldable1 f) => (y -> y -> y) -> f y -> y
foldl1' = foldlWith1' id

foldr1 :: (Foldable1 f) => (y -> y -> y) -> f y -> y
foldr1 = foldrWith1 id

instance Folds1 (->) (->) Identity where
  foldWith1 :: (Semigroup z) => (x -> z) -> Identity x -> z
  foldWith1 x_z (Identity x) = x_z x
instance Folds1 (->) (->) List1 where
  foldWith1 :: (Semigroup z) => (x -> z) -> List1 x -> z
  foldWith1 x_z = \case
    Sole x -> x_z x
    x :|| xs -> x_z x <> foldWith1 x_z xs
instance Folds1 (->) (->) ((,) y) where
  foldWith1 :: (Semigroup z) => (x -> z) -> (y, x) -> z
  foldWith1 x_z (_, x) = x_z x
instance Folds1 (->) (->) Complex where
  foldWith1 :: (Semigroup z) => (x -> z) -> Complex x -> z
  foldWith1 x_z (xr :+ xi) = x_z xr <> x_z xi
instance Folds1 (->) (->) (Arg y) where
  foldWith1 :: (Semigroup z) => (x -> z) -> Arg y x -> z
  foldWith1 x_z (Arg _ x) = x_z x
instance
  (Foldable1 f, Foldable1 g) =>
  Folds1 (->) (->) (Product f g)
  where
  foldWith1 :: (Semigroup z) => (x -> z) -> Product f g x -> z
  foldWith1 x_z (Pair fx gx) =
    foldWith1 x_z fx <> foldWith1 x_z gx
instance
  (Foldable1 f, Foldable1 g) =>
  Folds1 (->) (->) (Sum f g)
  where
  foldWith1 :: (Semigroup z) => (x -> z) -> Sum f g x -> z
  foldWith1 x_z = \case
    InL fx -> foldWith1 x_z fx
    InR gx -> foldWith1 x_z gx
instance
  (Foldable1 f, Foldable1 g) =>
  Folds1 (->) (->) (Compose f g)
  where
  foldWith1 ::
    forall x z.
    (Semigroup z) => (x -> z) -> Compose f g x -> z
  foldWith1 x_z (Compose fgx) =
    foldWith1 (foldWith1 x_z :: g x -> z) fgx

instance Folds1 (->) (->) V1 where
  foldWith1 :: (Semigroup z) => (x -> z) -> V1 x -> z
  foldWith1 _ v = case v of {}
instance Folds1 (->) (->) Par1 where
  foldWith1 :: (Semigroup z) => (x -> z) -> Par1 x -> z
  foldWith1 x_z (Par1 x) = x_z x
instance (Foldable1 f) => Folds1 (->) (->) (Rec1 f) where
  foldWith1 :: (Semigroup z) => (x -> z) -> Rec1 f x -> z
  foldWith1 x_z (Rec1 fx) = foldWith1 x_z fx
instance (Foldable1 f) => Folds1 (->) (->) (M1 i c f) where
  foldWith1 :: (Semigroup z) => (x -> z) -> M1 i c f x -> z
  foldWith1 x_z (M1 fx) = foldWith1 x_z fx
instance (Foldable1 f, Foldable1 g) => Folds1 (->) (->) (f :*: g) where
  foldWith1 :: (Semigroup z) => (x -> z) -> (f :*: g) x -> z
  foldWith1 x_z (fx :*: gx) = foldWith1 x_z fx <> foldWith1 x_z gx
instance (Foldable1 f, Foldable1 g) => Folds1 (->) (->) (f :+: g) where
  foldWith1 :: (Semigroup z) => (x -> z) -> (f :+: g) x -> z
  foldWith1 x_z = \case
    L1 fx -> foldWith1 x_z fx
    R1 gx -> foldWith1 x_z gx
instance (Foldable1 f, Foldable1 g) => Folds1 (->) (->) (f :.: g) where
  foldWith1 ::
    forall x z. (Semigroup z) => (x -> z) -> (f :.: g) x -> z
  foldWith1 x_z (Comp1 fgx) =
    foldWith1 (foldWith1 x_z :: g x -> z) fgx

type IxFoldable1 i f = (Folds1 (->) (->) f, Folds1 (Ix i) (->) f)

ifoldWith1 :: (IxFoldable1 i f, Semigroup z) => (i -> x -> z) -> f x -> z
ifoldWith1 = foldWith1 . Ix

ifoldl1' :: (IxFoldable1 i f) => (i -> y -> x -> y) -> y -> f x -> y
ifoldl1' i_y_x_y y0 fx =
  ifoldr1
    (\i x y_y -> oneShot \ !y -> y_y (i_y_x_y i y x))
    id
    fx
    y0
ifoldr1 :: (IxFoldable1 i f) => (i -> x -> y -> y) -> y -> f x -> y
ifoldr1 i_x_y_y y fx =
  foldWith1 (Ix \i -> Endo . i_x_y_y i) fx `appEndo` y

instance Folds1 (Ix ()) (->) Identity where
  foldWith1 ::
    (Semigroup z) =>
    Ix () x z -> Identity x -> z
  foldWith1 (Ix u_x_z) (Identity x) = u_x_z () x
instance Folds1 (Ix Natural) (->) List1 where
  foldWith1 ::
    (Semigroup z) =>
    Ix Natural x z -> List1 x -> z
  foldWith1 (Ix i_x_z) = flip fix 0 \rec i -> \case
    Sole x -> i_x_z i x
    x :|| xs -> i_x_z i x <> rec (succ i) xs
instance Folds1 (Ix y) (->) ((,) y) where
  foldWith1 :: (Semigroup z) => Ix y x z -> (y, x) -> z
  foldWith1 (Ix y_x_z) (y, x) = y_x_z y x
instance Folds1 (Ix (Either () ())) (->) Complex where
  foldWith1 ::
    (Semigroup z) =>
    Ix (Either () ()) x z -> Complex x -> z
  foldWith1 (Ix e_x_z) (xr :+ xi) =
    e_x_z (Left ()) xr <> e_x_z (Right ()) xi
instance Folds1 (Ix y) (->) (Arg y) where
  foldWith1 :: (Semigroup z) => Ix y x z -> Arg y x -> z
  foldWith1 (Ix y_x_z) (Arg y x) = y_x_z y x
instance
  (IxFoldable1 i f, IxFoldable1 j g) =>
  Folds1 (Ix (Either i j)) (->) (Product f g)
  where
  foldWith1 ::
    (Semigroup z) =>
    Ix (Either i j) x z -> Product f g x -> z
  foldWith1 (Ix eij_x_z) (Pair fx gx) =
    foldWith1 (Ix (eij_x_z . Left)) fx
      <> foldWith1 (Ix (eij_x_z . Right)) gx
instance
  (IxFoldable1 i f, IxFoldable1 j g) =>
  Folds1 (Ix (Either i j)) (->) (Sum f g)
  where
  foldWith1 ::
    (Semigroup z) =>
    Ix (Either i j) x z -> Sum f g x -> z
  foldWith1 (Ix eij_x_z) = \case
    InL fx -> foldWith1 (Ix (eij_x_z . Left)) fx
    InR gx -> foldWith1 (Ix (eij_x_z . Right)) gx
instance
  (IxFoldable1 i f, IxFoldable1 j g) =>
  Folds1 (Ix (i, j)) (->) (Compose f g)
  where
  foldWith1 ::
    (Semigroup z) =>
    Ix (i, j) x z -> Compose f g x -> z
  foldWith1 (Ix ij_x_z) (Compose fgx) =
    foldWith1
      (Ix \i -> foldWith1 (Ix \j -> ij_x_z (i, j)))
      fgx

instance Folds1 (Ix Void) (->) V1 where
  foldWith1 :: (Semigroup z) => Ix Void x z -> V1 x -> z
  foldWith1 _ v = case v of {}
instance Folds1 (Ix ()) (->) Par1 where
  foldWith1 :: (Semigroup z) => Ix () x z -> Par1 x -> z
  foldWith1 (Ix u_x_z) (Par1 x) = u_x_z () x
instance (IxFoldable1 i f) => Folds1 (Ix i) (->) (Rec1 f) where
  foldWith1 :: (Semigroup z) => Ix i x z -> Rec1 f x -> z
  foldWith1 (Ix i_x_z) (Rec1 fx) =
    foldWith1 (Ix i_x_z) fx
instance (IxFoldable1 j f) => Folds1 (Ix j) (->) (M1 i c f) where
  foldWith1 :: (Semigroup z) => Ix j x z -> M1 i c f x -> z
  foldWith1 (Ix j_x_z) (M1 fx) =
    foldWith1 (Ix j_x_z) fx
instance
  (IxFoldable1 i f, IxFoldable1 j g) =>
  Folds1 (Ix (Either i j)) (->) (f :*: g)
  where
  foldWith1 ::
    (Semigroup z) =>
    Ix (Either i j) x z -> (f :*: g) x -> z
  foldWith1 (Ix eij_x_z) (fx :*: gx) =
    foldWith1 (Ix (eij_x_z . Left)) fx
      <> foldWith1 (Ix (eij_x_z . Right)) gx
instance
  (IxFoldable1 i f, IxFoldable1 j g) =>
  Folds1 (Ix (Either i j)) (->) (f :+: g)
  where
  foldWith1 ::
    (Semigroup z) =>
    Ix (Either i j) x z -> (f :+: g) x -> z
  foldWith1 (Ix eij_x_z) = \case
    L1 fx -> foldWith1 (Ix (eij_x_z . Left)) fx
    R1 gx -> foldWith1 (Ix (eij_x_z . Right)) gx
instance
  (IxFoldable1 i f, IxFoldable1 j g) =>
  Folds1 (Ix (i, j)) (->) (f :.: g)
  where
  foldWith1 ::
    (Semigroup z) =>
    Ix (i, j) x z -> (f :.: g) x -> z
  foldWith1 (Ix ij_x_z) (Comp1 fgx) =
    foldWith1
      (Ix \i -> foldWith1 (Ix \j -> ij_x_z (i, j)))
      fgx

type Against f = Morphisms Op (->) f

instance Morphisms Op (->) (Op z) where
  morphism :: Op x y -> Op z x -> Op z y
  morphism (Op y_x) (Op x_z) = Op (x_z . y_x)
instance Morphisms Op (->) Proxy where
  morphism :: Op x y -> Proxy x -> Proxy y
  morphism (Op _) Proxy = Proxy
instance Morphisms Op (->) (Const z) where
  morphism :: Op x y -> Const z x -> Const z y
  morphism (Op _) (Const z) = Const z
instance
  (Against f, Against g) =>
  Morphisms Op (->) (Product f g)
  where
  morphism :: Op x y -> Product f g x -> Product f g y
  morphism (Op y_x) (Pair fx gx) =
    Pair (morphism (Op y_x) fx) (morphism (Op y_x) gx)
instance (Against f, Against g) => Morphisms Op (->) (Sum f g) where
  morphism :: Op x y -> Sum f g x -> Sum f g y
  morphism (Op y_x) = \case
    InL fx -> InL (morphism (Op y_x) fx)
    InR gx -> InR (morphism (Op y_x) gx)
instance (Along f, Against g) => Morphisms Op (->) (Compose f g) where
  morphism :: forall x y. Op x y -> Compose f g x -> Compose f g y
  morphism (Op y_x) (Compose fgx) =
    Compose (morphism (morphism (Op y_x) :: g x -> g y) fgx)

instance Morphisms Op (->) V1 where
  morphism :: Op x y -> V1 x -> V1 y
  morphism (Op _) v = case v of {}
instance Morphisms Op (->) U1 where
  morphism :: Op x y -> U1 x -> U1 y
  morphism (Op _) U1 = U1
instance Morphisms Op (->) (K1 i c) where
  morphism :: Op x y -> K1 i c x -> K1 i c y
  morphism (Op _) (K1 c) = K1 c
instance (Against f) => Morphisms Op (->) (M1 i c f) where
  morphism :: Op x y -> M1 i c f x -> M1 i c f y
  morphism (Op y_x) (M1 fx) = M1 (morphism (Op y_x) fx)
instance (Against f, Against g) => Morphisms Op (->) (f :*: g) where
  morphism :: Op x y -> (f :*: g) x -> (f :*: g) y
  morphism (Op y_x) (fx :*: gx) =
    morphism (Op y_x) fx :*: morphism (Op y_x) gx
instance (Against f, Against g) => Morphisms Op (->) (f :+: g) where
  morphism :: Op x y -> (f :+: g) x -> (f :+: g) y
  morphism (Op y_x) = \case
    L1 fx -> L1 (morphism (Op y_x) fx)
    R1 gx -> R1 (morphism (Op y_x) gx)
instance (Along f, Against g) => Morphisms Op (->) (f :.: g) where
  morphism :: forall x y. Op x y -> (f :.: g) x -> (f :.: g) y
  morphism (Op y_x) (Comp1 fgx) =
    Comp1 (morphism (morphism (Op y_x) :: g x -> g y) fgx)

type Phantom f = (Along f, Against f)
phantom :: (Phantom f) => f x -> f y
phantom fx = morphism (Op (const ())) (morphism (const ()) fx)

type Transform ::
  (j -> j -> Type) ->
  (k -> k -> Type) ->
  (j -> k) ->
  (j -> k) ->
  Type
newtype Transform c d f g = Transform
  {transform :: forall x. (Objects c x) => d (f x) (g x)}
type (-->) = Transform (->) (->)

instance (Category c, Category d) => Category (Transform c d) where
  type Objects (Transform c d) = Composed (Objects c) (Objects d)
  id :: (Objects (Transform c d) f) => Transform c d f f
  id = Transform id
  (.) ::
    ( Objects (Transform c d) f
    , Objects (Transform c d) g
    , Objects (Transform c d) h
    ) =>
    Transform c d g h -> Transform c d f g -> Transform c d f h
  Transform g_h . Transform f_g = Transform (g_h . f_g)
instance (Control.Category c, Control.Category d) => Control.Category (Transform c d) where
  id :: Transform c d f f
  id = Transform Control.id
  (.) :: Transform c d g h -> Transform c d f g -> Transform c d f h
  Transform g_h . Transform f_g = Transform (g_h Control.. f_g)

type Along2 :: (Type -> Type -> Type) -> Constraint
type Along2 f = (forall z. Along (f z), Morphisms (->) (-->) f)

morphism' ::
  forall b x x'.
  (Morphisms (->) (-->) b) =>
  (x -> x') -> (forall y. b x y -> b x' y)
morphism' x_x' = transform (morphism x_x' :: b x --> b x')

along2 :: (Along2 b) => (x -> x') -> (y -> y') -> b x y -> b x' y'
along2 x_x' y_y' bxy = morphism y_y' (morphism' x_x' bxy)

instance Morphisms (->) (-->) Either where
  morphism :: (x -> y) -> Either x --> Either y
  morphism x_y = Transform \case
    Left x -> Left (x_y x)
    Right z -> Right z
instance Morphisms (->) (-->) (,) where
  morphism :: (x -> y) -> (,) x --> (,) y
  morphism x_y = Transform \(x, z) -> (x_y x, z)
instance Morphisms (->) (-->) Arg where
  morphism :: (x -> y) -> Arg x --> Arg y
  morphism x_y = Transform \(Arg x z) -> Arg (x_y x) z
instance Morphisms (->) (-->) Const where
  morphism :: (x -> y) -> Const x --> Const y
  morphism x_y = Transform \(Const x) -> Const (x_y x)
instance Morphisms (->) (-->) ((,,) a) where
  morphism :: (x -> y) -> (,,) a x --> (,,) a y
  morphism x_y =
    Transform \(a, x, c) -> (a, x_y x, c)
instance Morphisms (->) (-->) ((,,,) a b) where
  morphism :: (x -> y) -> (,,,) a b x --> (,,,) a b y
  morphism x_y =
    Transform \(a, b, x, d) -> (a, b, x_y x, d)
instance Morphisms (->) (-->) ((,,,,) a b c) where
  morphism :: (x -> y) -> (,,,,) a b c x --> (,,,,) a b c y
  morphism x_y =
    Transform \(a, b, c, x, e) -> (a, b, c, x_y x, e)
instance Morphisms (->) (-->) ((,,,,,) a b c d) where
  morphism :: (x -> y) -> (,,,,,) a b c d x --> (,,,,,) a b c d y
  morphism x_y =
    Transform \(a, b, c, d, x, f) -> (a, b, c, d, x_y x, f)
instance Morphisms (->) (-->) ((,,,,,,) a b c d e) where
  morphism ::
    (x -> y) -> (,,,,,,) a b c d e x --> (,,,,,,) a b c d e y
  morphism x_y =
    Transform \(a, b, c, d, e, x, g) -> (a, b, c, d, e, x_y x, g)
instance Morphisms (->) (-->) ((,,,,,,,) a b c d e f) where
  morphism ::
    (x -> y) -> (,,,,,,,) a b c d e f x --> (,,,,,,,) a b c d e f y
  morphism x_y =
    Transform \(a, b, c, d, e, f, x, h) -> (a, b, c, d, e, f, x_y x, h)

instance Morphisms (->) (-->) (K1 i) where
  morphism :: (c -> k) -> K1 i c --> K1 i k
  morphism c_k = Transform \(K1 c) -> K1 (c_k c)

instance Morphisms (~>) (-->) Map where
  morphism :: (x ~> y) -> Map x --> Map y
  morphism (OrdArrow x_y) = Transform (Map.mapKeys x_y)

type Apply :: (Type -> Type) -> Constraint
class (Along f) => Apply f where
  {-# MINIMAL (<*>) | liftA2 #-}
  (<*>) :: f (x -> y) -> f x -> f y
  (<*>) = liftA2 ($)
  liftA2 :: (Apply f) => (x -> y -> z) -> f x -> f y -> f z
  liftA2 xyz fx fy = morphism xyz fx <*> fy

liftA3 :: (Apply f) => (x -> y -> z -> w) -> f x -> f y -> f z -> f w
liftA3 xyzw fx fy fz = liftA2 xyzw fx fy <*> fz

(<*) :: (Apply f) => f x -> f y -> f x
fx <* fy = liftA2 (\ !x !_ -> x) fx fy

(*>) :: (Apply f) => f x -> f y -> f y
fx *> fy = liftA2 (\ !_ !y -> y) fx fy

instance Apply ((->) z) where
  (<*>) :: (z -> (x -> y)) -> (z -> x) -> z -> y
  zxy <*> zx = \z -> zxy z (zx z)
instance (Apply f) => Apply (Kleisli f z) where
  (<*>) :: Kleisli f z (x -> y) -> Kleisli f z x -> Kleisli f z y
  Kleisli zfxy <*> Kleisli z_fx = Kleisli \z -> zfxy z <*> z_fx z
instance Apply Identity where
  (<*>) :: Identity (x -> y) -> Identity x -> Identity y
  Identity f <*> Identity x = Identity (f x)
instance Apply Proxy where
  (<*>) :: Proxy (x -> y) -> Proxy x -> Proxy y
  Proxy <*> Proxy = Proxy
instance (Semigroup z) => Apply (Const z) where
  (<*>) :: Const z (x -> y) -> Const z x -> Const z y
  Const z0 <*> Const z = Const (z0 <> z)
instance Apply Maybe where
  (<*>) :: Maybe (x -> y) -> Maybe x -> Maybe y
  (<*>) = \cases
    Nothing _ -> Nothing
    (Just _) Nothing -> Nothing
    (Just x_y) (Just x) -> Just (x_y x)
  liftA2 :: (x -> y -> z) -> Maybe x -> Maybe y -> Maybe z
  liftA2 xyz = \cases
    (Just x) (Just y) -> Just (xyz x y)
    _ _ -> Nothing
instance Apply (Either l) where
  (<*>) :: Either l (x -> y) -> Either l x -> Either l y
  (<*>) = \cases
    (Left l) _ -> Left l
    (Right _) (Left l) -> Left l
    (Right x_y) (Right x) -> Right (x_y x)
  liftA2 :: (x -> y -> z) -> Either l x -> Either l y -> Either l z
  liftA2 xyz = \cases
    (Right x) (Right y) -> Right (xyz x y)
    (Left l) _ -> Left l
    _ (Left l) -> Left l
instance Apply List1 where
  (<*>) :: List1 (x -> y) -> List1 x -> List1 y
  (<*>) = (Control.<*>)
instance Apply List where
  (<*>) :: List (x -> y) -> List x -> List y
  (<*>) = (Control.<*>)
instance Apply Vector.Vector where
  (<*>) :: Vector.Vector (x -> y) -> Vector.Vector x -> Vector.Vector y
  (<*>) = (Control.<*>)
instance Apply Complex where
  (<*>) :: Complex (x -> y) -> Complex x -> Complex y
  (ry :+ iy) <*> (r :+ i) = ry r :+ iy i
instance (Semigroup a) => Apply ((,) a) where
  (<*>) :: (a, x -> y) -> (a, x) -> (a, y)
  (a, x_y) <*> (a', x) = (a <> a', x_y x)
instance (Semigroup a, Semigroup b) => Apply ((,,) a b) where
  (<*>) :: (a, b, x -> y) -> (a, b, x) -> (a, b, y)
  (a, b, x_y) <*> (a', b', x) = (a <> a', b <> b', x_y x)
instance
  (Semigroup a, Semigroup b, Semigroup c) =>
  Apply ((,,,) a b c)
  where
  (<*>) :: (a, b, c, x -> y) -> (a, b, c, x) -> (a, b, c, y)
  (a, b, c, x_y) <*> (a', b', c', x) =
    (a <> a', b <> b', c <> c', x_y x)
instance
  (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Apply ((,,,,) a b c d)
  where
  (<*>) :: (a, b, c, d, x -> y) -> (a, b, c, d, x) -> (a, b, c, d, y)
  (a, b, c, d, x_y) <*> (a', b', c', d', x) =
    (a <> a', b <> b', c <> c', d <> d', x_y x)
instance (Apply f, Apply g) => Apply (Product f g) where
  (<*>) :: Product f g (x -> y) -> Product f g x -> Product f g y
  Pair fxy gxy <*> Pair fx gx = Pair (fxy <*> fx) (gxy <*> gx)
instance (Apply f, Apply g) => Apply (Compose f g) where
  (<*>) :: Compose f g (x -> y) -> Compose f g x -> Compose f g y
  Compose fgxy <*> Compose fgx = Compose (liftA2 (<*>) fgxy fgx)
instance Apply IO where
  (<*>) :: IO (x -> y) -> IO x -> IO y
  (<*>) = Control.ap

instance Apply U1 where
  (<*>) :: U1 (x -> y) -> U1 x -> U1 y
  U1 <*> U1 = U1
instance Apply Par1 where
  (<*>) :: Par1 (x -> y) -> Par1 x -> Par1 y
  Par1 x_y <*> Par1 x = Par1 (x_y x)
instance (Apply f) => Apply (Rec1 f) where
  (<*>) :: Rec1 f (x -> y) -> Rec1 f x -> Rec1 f y
  Rec1 fxy <*> Rec1 fx = Rec1 (fxy <*> fx)
instance (Semigroup c) => Apply (K1 i c) where
  (<*>) :: K1 i c (x -> y) -> K1 i c x -> K1 i c y
  K1 c <*> K1 k = K1 (c <> k)
instance (Apply f) => Apply (M1 i c f) where
  (<*>) :: M1 i c f (x -> y) -> M1 i c f x -> M1 i c f y
  M1 x_y <*> M1 x = M1 (x_y <*> x)
instance (Apply f, Apply g) => Apply (f :*: g) where
  (<*>) :: (f :*: g) (x -> y) -> (f :*: g) x -> (f :*: g) y
  (fxy :*: gxy) <*> (fx :*: gx) = (fxy <*> fx) :*: (gxy <*> gx)
instance (Apply f, Apply g) => Apply (f :.: g) where
  (<*>) :: (f :.: g) (x -> y) -> (f :.: g) x -> (f :.: g) y
  Comp1 fgxy <*> Comp1 fgx = Comp1 (liftA2 (<*>) fgxy fgx)

type Pure :: (Type -> Type) -> Constraint
class Pure f where
  pure :: x -> f x

instance Pure ((->) z) where
  pure :: x -> (z -> x)
  pure = Data.Function.const
instance Pure Identity where
  pure :: x -> Identity x
  pure = Identity
instance Pure Proxy where
  pure :: x -> Proxy x
  pure _ = Proxy
instance (Monoid z) => Pure (Const z) where
  pure :: x -> Const z x
  pure _ = Const mempty
instance Pure Maybe where
  pure :: x -> Maybe x
  pure = Just
instance Pure (Either l) where
  pure :: x -> Either l x
  pure = Right
instance Pure List1 where
  pure :: x -> List1 x
  pure = (:| [])
instance Pure List where
  pure :: x -> List x
  pure = (: [])
instance Pure Vector.Vector where
  pure :: x -> Vector.Vector x
  pure = Vector.singleton
instance (Pure f, Pure g) => Pure (Product f g) where
  pure :: x -> Product f g x
  pure x = Pair (pure x) (pure x)
instance (Pure f, Pure g) => Pure (Compose f g) where
  pure :: x -> Compose f g x
  pure x = Compose (pure (pure x))
instance (Pure f) => Pure (Kleisli f z) where
  pure :: x -> Kleisli f z x
  pure x = Kleisli (const (pure x))
instance Pure IO where
  pure :: x -> IO x
  pure = returnIO

instance Pure Seq where
  pure :: x -> Seq x
  pure = Seq.singleton

instance Pure U1 where
  pure :: x -> U1 x
  pure _ = U1
instance Pure Par1 where
  pure :: x -> Par1 x
  pure = Par1
instance (Pure f) => Pure (Rec1 f) where
  pure :: x -> Rec1 f x
  pure = Rec1 . pure
instance (Monoid c) => Pure (K1 i c) where
  pure :: (Monoid c) => x -> K1 i c x
  pure _ = K1 mempty
instance (Pure f) => Pure (M1 i c f) where
  pure :: x -> M1 i c f x
  pure = M1 . pure
instance (Pure f, Pure g) => Pure (f :*: g) where
  pure :: x -> (f :*: g) x
  pure x = pure x :*: pure x
instance (Pure f, Pure g) => Pure (f :.: g) where
  pure :: x -> (f :.: g) x
  pure x = Comp1 (pure (pure x))

type Applicative :: (Type -> Type) -> Constraint
type Applicative f = (Pure f, Apply f)

newtype StateT s f x = StateT {runStateT :: s -> f (s, x)}
  deriving (Data.Functor)

type State s = StateT s Identity
state :: (s -> (s, x)) -> State s x
state f = StateT (Identity . f)
runState :: State s x -> s -> (s, x)
runState (StateT x) s = runIdentity (x s)

get :: (Pure f) => StateT s f s
get = StateT \s -> pure (s, s)

gets :: (Pure f) => (s -> x) -> StateT s f x
gets s_x = StateT \s -> pure (s, s_x s)

put :: (Pure f) => s -> StateT s f ()
put s = StateT \_ -> pure (s, ())

modify :: (Pure f) => (s -> s) -> StateT s f ()
modify s_s = StateT \s -> pure (s_s s, ())

instance (Along f) => Morphisms (->) (->) (StateT s f) where
  morphism :: forall x y. (x -> y) -> StateT s f x -> StateT s f y
  morphism x_y (StateT s_fsx) =
    StateT (morphism (morphism x_y :: (s, x) -> (s, y)) . s_fsx)
instance (Pure f) => Pure (StateT s f) where
  pure :: (Pure f) => x -> StateT s f x
  pure x = StateT \s -> pure (s, x)
instance (Monad f) => Apply (StateT s f) where
  (<*>) :: StateT s f (x -> y) -> StateT s f x -> StateT s f y
  StateT s_x_y <*> StateT s_x =
    StateT \s ->
      s_x s >>= \(s', x) ->
        s_x_y s' >>= \(s'', x_y) ->
          pure (s'', x_y x)
instance (Monad f) => Bind (StateT s f) where
  (>>=) :: StateT s f x -> (x -> StateT s f y) -> StateT s f y
  StateT s_x >>= x_Ssfy =
    StateT \s -> s_x s >>= \(s', x) -> (x_Ssfy x).runStateT s'
instance (Control.Monad f) => Control.Applicative (StateT s f) where
  pure :: x -> StateT s f x
  pure x = StateT \s -> Control.pure (s, x)
  (<*>) :: StateT s f (x -> y) -> StateT s f x -> StateT s f y
  StateT s_x_y <*> StateT s_x =
    StateT \s ->
      s_x s Control.>>= \(s', x) ->
        s_x_y s' Control.>>= \(s'', x_y) ->
          Control.pure (s'', x_y x)
instance (Control.Monad f) => Control.Monad (StateT s f) where
  (>>=) :: StateT s f x -> (x -> StateT s f y) -> StateT s f y
  StateT s_x >>= x_Ssfy =
    StateT \s ->
      s_x s Control.>>= \(s', x) -> (x_Ssfy x).runStateT s'

class (Morphisms c d f, Folds c d f) => Traversals c d f where
  traverse :: (Applicative g) => c x (g y) -> d (f x) (g (f y))

type Traversable f = Traversals (->) (->) f

sequence :: (Traversable f, Applicative g) => f (g x) -> g (f x)
sequence = traverse (id :: x -> x)

for :: (Traversable f, Applicative g) => f x -> (x -> g y) -> g (f y)
for = flip traverse

mapAccumM ::
  (Monad f, Traversable t) =>
  (s -> x -> f (s, y)) -> s -> t x -> f (s, t y)
mapAccumM s_x_fxsy s tx =
  traverse (StateT . flip s_x_fxsy) tx `runStateT` s

mapAccum :: (Traversable t) => (s -> x -> (s, y)) -> s -> t x -> (s, t y)
mapAccum f xs tx = runIdentity (mapAccumM ((Identity .) . f) xs tx)

instance Traversals (->) (->) Identity where
  traverse :: (Applicative g) => (x -> g y) -> Identity x -> g (Identity y)
  traverse x_gy (Identity x) = morphism Identity (x_gy x)
instance Traversals (->) (->) Maybe where
  traverse :: (Applicative g) => (x -> g y) -> Maybe x -> g (Maybe y)
  traverse x_gy = \case
    Nothing -> pure Nothing
    Just x -> morphism Just (x_gy x)
instance Traversals (->) (->) List1 where
  traverse :: (Applicative g) => (x -> g y) -> List1 x -> g (List1 y)
  traverse x_gy = \case
    Sole x -> morphism Sole (x_gy x)
    x :|| xs -> liftA2 (:||) (x_gy x) (traverse x_gy xs)
instance Traversals (->) (->) List where
  traverse :: (Applicative g) => (x -> g y) -> List x -> g (List y)
  traverse x_gy = \case
    [] -> pure []
    x : xs -> liftA2 (:) (x_gy x) (traverse x_gy xs)
instance Traversals (->) (->) ((,) z) where
  traverse :: (Applicative g) => (x -> g y) -> (z, x) -> g (z, y)
  traverse x_gy (z, x) = morphism (z,) (x_gy x)
instance Traversals (->) (->) Complex where
  traverse :: (Applicative g) => (x -> g y) -> Complex x -> g (Complex y)
  traverse x_gy (xr :+ xi) = liftA2 (:+) (x_gy xr) (x_gy xi)
instance Traversals (->) (->) (Either z) where
  traverse :: (Applicative g) => (x -> g y) -> Either z x -> g (Either z y)
  traverse x_gy = \case
    Left z -> pure (Left z)
    Right x -> morphism Right (x_gy x)
instance Traversals (->) (->) Proxy where
  traverse :: (Applicative g) => (x -> g y) -> Proxy x -> g (Proxy y)
  traverse _ Proxy = pure Proxy
instance Traversals (->) (->) (Const z) where
  traverse :: (Applicative g) => (x -> g y) -> Const z x -> g (Const z y)
  traverse _ (Const z) = pure (Const z)
instance (Traversable f, Traversable g) => Traversals (->) (->) (Product f g) where
  traverse :: (Applicative h) => (x -> h y) -> Product f g x -> h (Product f g y)
  traverse x_hy (Pair fx gx) =
    liftA2 Pair (traverse x_hy fx) (traverse x_hy gx)
instance (Traversable f, Traversable g) => Traversals (->) (->) (Sum f g) where
  traverse :: (Applicative h) => (x -> h y) -> Sum f g x -> h (Sum f g y)
  traverse x_hy = \case
    InL fx -> morphism InL (traverse x_hy fx)
    InR gx -> morphism InR (traverse x_hy gx)
instance (Traversable f, Traversable g) => Traversals (->) (->) (Compose f g) where
  traverse ::
    forall h x y.
    (Applicative h) => (x -> h y) -> Compose f g x -> h (Compose f g y)
  traverse x_hy (Compose fgx) =
    morphism Compose (traverse (traverse x_hy :: g x -> h (g y)) fgx)

instance (Array.Ix i) => Traversals (->) (->) (Array i) where
  traverse :: (Applicative g) => (x -> g y) -> Array i x -> g (Array i y)
  traverse x_gy arr =
    morphism
      (Array.listArray (Array.bounds arr))
      (traverse x_gy (Array.elems arr))
instance Traversals (->) (->) (Map k) where
  traverse :: (Applicative g) => (x -> g y) -> Map k x -> g (Map k y)
  traverse x_gy = \case
    Map.Tip -> pure Map.Tip
    Map.Bin 1 k v _ _ -> flip morphism (x_gy v) \v' -> Map.Bin 1 k v' Map.Tip Map.Tip
    Map.Bin s k v l r ->
      morphism (flip (Map.Bin s k)) (traverse x_gy l) <*> x_gy v <*> traverse x_gy r
instance Traversals (->) (->) IntMap where
  traverse :: (Applicative g) => (x -> g y) -> IntMap x -> g (IntMap y)
  traverse x_gy = \case
    IntMap.Nil -> pure IntMap.Nil
    (IntMap.Tip k v) -> morphism (IntMap.Tip k) (x_gy v)
    (IntMap.Bin p l r)
      | Internal.signBranch p ->
          liftA2 (flip (IntMap.Bin p)) (traverse x_gy r) (traverse x_gy l)
      | otherwise ->
          liftA2 (IntMap.Bin p) (traverse x_gy l) (traverse x_gy r)
instance Traversals (->) (->) Seq where
  traverse :: (Applicative g) => (x -> g y) -> Seq x -> g (Seq y)
  traverse x_gy = \case
    Seq.Empty -> pure Seq.Empty
    x Seq.:<| xs -> liftA2 (Seq.:<|) (x_gy x) (traverse x_gy xs)

instance Traversals (->) (->) Vector.Vector where
  traverse ::
    (Applicative g) => (x -> g y) -> Vector.Vector x -> g (Vector.Vector y)
  traverse x_gy v =
    let !n = Vector.length v
     in morphism (Vector.fromListN n) (traverse x_gy (Vector.toList v))

instance Traversals (->) (->) V1 where
  traverse :: (Applicative g) => (x -> g y) -> V1 x -> g (V1 y)
  traverse _ v = case v of {}
instance Traversals (->) (->) U1 where
  traverse :: (Applicative g) => (x -> g y) -> U1 x -> g (U1 y)
  traverse _ U1 = pure U1
instance Traversals (->) (->) Par1 where
  traverse :: (Applicative g) => (x -> g y) -> Par1 x -> g (Par1 y)
  traverse x_gy (Par1 x) = morphism Par1 (x_gy x)
instance (Traversable f) => Traversals (->) (->) (Rec1 f) where
  traverse :: (Applicative g) => (x -> g y) -> Rec1 f x -> g (Rec1 f y)
  traverse x_gy (Rec1 fx) = morphism Rec1 (traverse x_gy fx)
instance Traversals (->) (->) (K1 i c) where
  traverse :: (Applicative g) => (x -> g y) -> K1 i c x -> g (K1 i c y)
  traverse _ (K1 c) = pure (K1 c)
instance (Traversable f) => Traversals (->) (->) (M1 i c f) where
  traverse :: (Applicative g) => (x -> g y) -> M1 i c f x -> g (M1 i c f y)
  traverse x_gy (M1 fx) = morphism M1 (traverse x_gy fx)
instance (Traversable f, Traversable g) => Traversals (->) (->) (f :*: g) where
  traverse :: (Applicative h) => (x -> h y) -> (f :*: g) x -> h ((f :*: g) y)
  traverse x_hy (fx :*: gx) =
    liftA2 (:*:) (traverse x_hy fx) (traverse x_hy gx)
instance (Traversable f, Traversable g) => Traversals (->) (->) (f :+: g) where
  traverse :: (Applicative h) => (x -> h y) -> (f :+: g) x -> h ((f :+: g) y)
  traverse x_hy = \case
    L1 fx -> morphism L1 (traverse x_hy fx)
    R1 gx -> morphism R1 (traverse x_hy gx)
instance (Traversable f, Traversable g) => Traversals (->) (->) (f :.: g) where
  traverse ::
    forall h x y.
    (Applicative h) => (x -> h y) -> (f :.: g) x -> h ((f :.: g) y)
  traverse x_hy (Comp1 fgx) =
    morphism Comp1 (traverse (traverse x_hy :: g x -> h (g y)) fgx)

type IxTraversable i f =
  (Traversals (->) (->) f, Traversals (Ix i) (->) f)

itraverse ::
  (IxTraversable i f, Applicative g) => (i -> x -> g y) -> f x -> g (f y)
itraverse i_x_gy fx = traverse (Ix i_x_gy) fx

ifor :: (IxTraversable i f, Applicative g) => f x -> (i -> x -> g y) -> g (f y)
ifor = flip itraverse

instance Traversals (Ix ()) (->) Identity where
  traverse ::
    (Applicative g) =>
    Ix () x (g y) -> Identity x -> g (Identity y)
  traverse (Ix u_x_gy) (Identity x) =
    morphism Identity (u_x_gy () x)
instance Traversals (Ix ()) (->) Maybe where
  traverse ::
    (Applicative g) =>
    Ix () x (g y) -> Maybe x -> g (Maybe y)
  traverse (Ix u_x_gy) = \case
    Nothing -> pure Nothing
    Just x -> morphism Just (u_x_gy () x)
instance Traversals (Ix Int) (->) List1 where
  traverse ::
    (Applicative g) =>
    Ix Int x (g y) -> List1 x -> g (List1 y)
  traverse (Ix i_x_gy) = flip fix 0 \rec i -> \case
    Sole x -> morphism Sole (i_x_gy i x)
    x :|| xs -> liftA2 (:||) (i_x_gy i x) (rec (succ i) xs)
instance Traversals (Ix Integer) (->) List1 where
  traverse ::
    (Applicative g) =>
    Ix Integer x (g y) -> List1 x -> g (List1 y)
  traverse (Ix i_x_gy) = flip fix 0 \rec i -> \case
    Sole x -> morphism Sole (i_x_gy i x)
    x :|| xs -> liftA2 (:||) (i_x_gy i x) (rec (succ i) xs)
instance Traversals (Ix Natural) (->) List1 where
  traverse ::
    (Applicative g) =>
    Ix Natural x (g y) -> List1 x -> g (List1 y)
  traverse (Ix i_x_gy) = flip fix 0 \rec i -> \case
    Sole x -> morphism Sole (i_x_gy i x)
    x :|| xs -> liftA2 (:||) (i_x_gy i x) (rec (succ i) xs)
instance Traversals (Ix Int) (->) List where
  traverse ::
    (Applicative g) =>
    Ix Int x (g y) -> List x -> g (List y)
  traverse (Ix i_x_gy) = flip fix 0 \rec i -> \case
    [] -> pure []
    x : xs -> liftA2 (:) (i_x_gy i x) (rec (succ i) xs)
instance Traversals (Ix Integer) (->) List where
  traverse ::
    (Applicative g) =>
    Ix Integer x (g y) -> List x -> g (List y)
  traverse (Ix i_x_gy) = flip fix 0 \rec i -> \case
    [] -> pure []
    x : xs -> liftA2 (:) (i_x_gy i x) (rec (succ i) xs)
instance Traversals (Ix Natural) (->) List where
  traverse ::
    (Applicative g) =>
    Ix Natural x (g y) -> List x -> g (List y)
  traverse (Ix i_x_gy) = flip fix 0 \rec i -> \case
    [] -> pure []
    x : xs -> liftA2 (:) (i_x_gy i x) (rec (succ i) xs)
instance Traversals (Ix z) (->) ((,) z) where
  traverse ::
    (Applicative g) =>
    Ix z x (g y) -> (z, x) -> g (z, y)
  traverse (Ix z_x_gy) (z, x) = morphism (z,) (z_x_gy z x)
instance Traversals (Ix Void) (->) Proxy where
  traverse ::
    (Applicative g) =>
    Ix Void x (g y) -> Proxy x -> g (Proxy y)
  traverse (Ix _) Proxy = pure Proxy
instance Traversals (Ix Void) (->) (Const z) where
  traverse ::
    (Applicative g) =>
    Ix Void x (g y) -> Const z x -> g (Const z y)
  traverse (Ix _) (Const z) = pure (Const z)
instance
  (IxTraversable i f, IxTraversable j g) =>
  Traversals (Ix (Either i j)) (->) (Product f g)
  where
  traverse ::
    (Applicative h) =>
    Ix (Either i j) x (h y) ->
    Product f g x ->
    h (Product f g y)
  traverse (Ix eij_x_hy) (Pair fx gx) =
    liftA2
      Pair
      (traverse (Ix (eij_x_hy . Left)) fx)
      (traverse (Ix (eij_x_hy . Right)) gx)
instance
  (IxTraversable i f, IxTraversable j g) =>
  Traversals (Ix (Either i j)) (->) (Sum f g)
  where
  traverse ::
    (Applicative h) =>
    Ix (Either i j) x (h y) ->
    Sum f g x ->
    h (Sum f g y)
  traverse (Ix eij_x_hy) = \case
    InL fx -> morphism InL (traverse (Ix (eij_x_hy . Left)) fx)
    InR gx -> morphism InR (traverse (Ix (eij_x_hy . Right)) gx)
instance
  (IxTraversable i f, IxTraversable j g) =>
  Traversals (Ix (i, j)) (->) (Compose f g)
  where
  traverse ::
    (Applicative h) =>
    Ix (i, j) x (h y) ->
    Compose f g x ->
    h (Compose f g y)
  traverse (Ix ij_x_hy) (Compose fgx) = morphism Compose do
    traverse
      (Ix \i -> traverse (Ix \j -> ij_x_hy (i, j)))
      fgx

instance (Array.Ix i) => Traversals (Ix i) (->) (Array i) where
  traverse :: (Applicative g) => Ix i x (g y) -> Array i x -> g (Array i y)
  traverse (Ix i_x_gy) arr =
    morphism
      (Array.listArray (Array.bounds arr))
      (traverse (uncurry i_x_gy) (Array.assocs arr))
instance Traversals (Ix k) (->) (Map k) where
  traverse :: (Applicative g) => Ix k x (g y) -> Map k x -> g (Map k y)
  traverse (Ix k_x_gy) = \case
    Map.Tip -> pure Map.Tip
    Map.Bin 1 k v _ _ -> flip morphism (k_x_gy k v) \v' -> Map.Bin 1 k v' Map.Tip Map.Tip
    Map.Bin s k v l r ->
      morphism (flip (Map.Bin s k)) (traverse (Ix k_x_gy) l)
        <*> k_x_gy k v
        <*> traverse (Ix k_x_gy) r
instance Traversals (Ix Int) (->) IntMap where
  traverse :: (Applicative g) => Ix Int x (g y) -> IntMap x -> g (IntMap y)
  traverse (Ix i_x_gy) = \case
    IntMap.Nil -> pure IntMap.Nil
    (IntMap.Tip i v) -> morphism (IntMap.Tip i) (i_x_gy i v)
    (IntMap.Bin p l r)
      | Internal.signBranch p ->
          liftA2 (flip (IntMap.Bin p)) (traverse (Ix i_x_gy) r) (traverse (Ix i_x_gy) l)
      | otherwise ->
          liftA2 (IntMap.Bin p) (traverse (Ix i_x_gy) l) (traverse (Ix i_x_gy) r)
instance Traversals (Ix Int) (->) Seq where
  traverse :: (Applicative g) => Ix Int x (g y) -> Seq x -> g (Seq y)
  traverse (Ix i_x_gy) = flip fix 0 \rec i -> \case
    Seq.Empty -> pure Seq.Empty
    x Seq.:<| xs -> liftA2 (Seq.:<|) (i_x_gy i x) (rec (succ i) xs)
instance Traversals (Ix Integer) (->) Seq where
  traverse :: (Applicative g) => Ix Integer x (g y) -> Seq x -> g (Seq y)
  traverse (Ix i_x_gy) = flip fix 0 \rec i -> \case
    Seq.Empty -> pure Seq.Empty
    x Seq.:<| xs -> liftA2 (Seq.:<|) (i_x_gy i x) (rec (succ i) xs)
instance Traversals (Ix Natural) (->) Seq where
  traverse :: (Applicative g) => Ix Natural x (g y) -> Seq x -> g (Seq y)
  traverse (Ix i_x_gy) = flip fix 0 \rec i -> \case
    Seq.Empty -> pure Seq.Empty
    x Seq.:<| xs -> liftA2 (Seq.:<|) (i_x_gy i x) (rec (succ i) xs)

instance Traversals (Ix Int) (->) Vector.Vector where
  traverse ::
    (Applicative g) => Ix Int x (g y) -> Vector.Vector x -> g (Vector.Vector y)
  traverse (Ix i_x_gy) v =
    let !n = Vector.length v
     in morphism (Vector.fromListN n) (traverse (Ix i_x_gy) (Vector.toList v))
instance Traversals (Ix Integer) (->) Vector.Vector where
  traverse ::
    (Applicative g) => Ix Integer x (g y) -> Vector.Vector x -> g (Vector.Vector y)
  traverse (Ix i_x_gy) v =
    let !n = Vector.length v
     in morphism (Vector.fromListN n) (traverse (Ix i_x_gy) (Vector.toList v))
instance Traversals (Ix Natural) (->) Vector.Vector where
  traverse ::
    (Applicative g) => Ix Natural x (g y) -> Vector.Vector x -> g (Vector.Vector y)
  traverse (Ix i_x_gy) v =
    let !n = Vector.length v
     in morphism (Vector.fromListN n) (traverse (Ix i_x_gy) (Vector.toList v))

instance Traversals (Ix Void) (->) V1 where
  traverse :: (Applicative g) => Ix Void x (g y) -> V1 x -> g (V1 y)
  traverse (Ix _) v = case v of {}
instance Traversals (Ix Void) (->) U1 where
  traverse :: (Applicative g) => Ix Void x (g y) -> U1 x -> g (U1 y)
  traverse (Ix _) U1 = pure U1
instance Traversals (Ix ()) (->) Par1 where
  traverse ::
    (Applicative g) =>
    Ix () x (g y) -> Par1 x -> g (Par1 y)
  traverse (Ix u_x_gy) (Par1 x) =
    morphism Par1 (u_x_gy () x)
instance (IxTraversable i f) => Traversals (Ix i) (->) (Rec1 f) where
  traverse ::
    (Applicative g) =>
    Ix i x (g y) -> Rec1 f x -> g (Rec1 f y)
  traverse (Ix i_x_gy) (Rec1 fx) =
    morphism Rec1 (traverse (Ix i_x_gy) fx)
instance Traversals (Ix Void) (->) (K1 i c) where
  traverse ::
    (Applicative g) =>
    Ix Void x (g y) -> K1 i c x -> g (K1 i c y)
  traverse (Ix _) (K1 c) = pure (K1 c)
instance
  (IxTraversable i f, IxTraversable j g) =>
  Traversals (Ix (Either i j)) (->) (f :*: g)
  where
  traverse ::
    (Applicative h) =>
    Ix (Either i j) x (h y) ->
    (f :*: g) x ->
    h ((f :*: g) y)
  traverse (Ix eij_x_hy) (fx :*: gx) =
    liftA2
      (:*:)
      (traverse (Ix (eij_x_hy . Left)) fx)
      (traverse (Ix (eij_x_hy . Right)) gx)
instance
  (IxTraversable i f, IxTraversable j g) =>
  Traversals (Ix (Either i j)) (->) (f :+: g)
  where
  traverse ::
    (Applicative h) =>
    Ix (Either i j) x (h y) ->
    (f :+: g) x ->
    h ((f :+: g) y)
  traverse (Ix eij_x_hy) = \case
    L1 fx -> morphism L1 (traverse (Ix (eij_x_hy . Left)) fx)
    R1 gx -> morphism R1 (traverse (Ix (eij_x_hy . Right)) gx)
instance
  (IxTraversable i f, IxTraversable j g) =>
  Traversals (Ix (i, j)) (->) (f :.: g)
  where
  traverse ::
    (Applicative h) =>
    Ix (i, j) x (h y) ->
    (f :.: g) x ->
    h ((f :.: g) y)
  traverse (Ix ij_x_hy) (Comp1 fgx) = morphism Comp1 do
    traverse
      (Ix \i -> traverse (Ix \j -> ij_x_hy (i, j)))
      fgx

class (Morphisms c d f, Folds1 c d f) => Traversals1 c d f where
  traverse1 :: (Apply g) => c x (g y) -> d (f x) (g (f y))

type Traversable1 f = Traversals1 (->) (->) f

sequence1 :: (Traversable1 f, Apply g) => f (g x) -> g (f x)
sequence1 = traverse1 (id :: x -> x)

for1 :: (Traversable1 f, Apply g) => f x -> (x -> g y) -> g (f y)
for1 = flip traverse1

instance Traversals1 (->) (->) Identity where
  traverse1 :: (Apply g) => (x -> g y) -> Identity x -> g (Identity y)
  traverse1 x_gy (Identity x) = morphism Identity (x_gy x)
instance Traversals1 (->) (->) List1 where
  traverse1 :: (Apply g) => (x -> g y) -> List1 x -> g (List1 y)
  traverse1 x_gy = \case
    Sole x -> morphism Sole (x_gy x)
    x :|| xs -> liftA2 (:||) (x_gy x) (traverse1 x_gy xs)
instance Traversals1 (->) (->) ((,) z) where
  traverse1 :: (Apply g) => (x -> g y) -> (z, x) -> g (z, y)
  traverse1 x_gy (z, x) = morphism (z,) (x_gy x)
instance Traversals1 (->) (->) Complex where
  traverse1 ::
    (Apply g) => (x -> g y) -> Complex x -> g (Complex y)
  traverse1 x_gy (xr :+ xi) = liftA2 (:+) (x_gy xr) (x_gy xi)
instance
  (Traversable1 f, Traversable1 g) =>
  Traversals1 (->) (->) (Product f g)
  where
  traverse1 ::
    (Apply h) =>
    (x -> h y) -> Product f g x -> h (Product f g y)
  traverse1 x_hy (Pair fx gx) =
    liftA2 Pair (traverse1 x_hy fx) (traverse1 x_hy gx)
instance
  (Traversable1 f, Traversable1 g) =>
  Traversals1 (->) (->) (Sum f g)
  where
  traverse1 ::
    (Apply h) =>
    (x -> h y) -> Sum f g x -> h (Sum f g y)
  traverse1 x_hy = \case
    InL fx -> morphism InL (traverse1 x_hy fx)
    InR gx -> morphism InR (traverse1 x_hy gx)
instance
  (Traversable1 f, Traversable1 g) =>
  Traversals1 (->) (->) (Compose f g)
  where
  traverse1 ::
    forall h x y.
    (Apply h) =>
    (x -> h y) -> Compose f g x -> h (Compose f g y)
  traverse1 x_hy (Compose fgx) = morphism Compose do
    traverse1
      (traverse1 x_hy :: g x -> h (g y))
      fgx

instance Traversals1 (->) (->) Par1 where
  traverse1 :: (Apply g) => (x -> g y) -> Par1 x -> g (Par1 y)
  traverse1 x_gy (Par1 x) = morphism Par1 (x_gy x)
instance (Traversable1 f) => Traversals1 (->) (->) (Rec1 f) where
  traverse1 :: (Apply g) => (x -> g y) -> Rec1 f x -> g (Rec1 f y)
  traverse1 x_gy (Rec1 fx) = morphism Rec1 (traverse1 x_gy fx)
instance (Traversable1 f) => Traversals1 (->) (->) (M1 i c f) where
  traverse1 :: (Apply g) => (x -> g y) -> M1 i c f x -> g (M1 i c f y)
  traverse1 x_gy (M1 fx) = morphism M1 (traverse1 x_gy fx)
instance
  (Traversable1 f, Traversable1 g) =>
  Traversals1 (->) (->) (f :*: g)
  where
  traverse1 ::
    (Apply h) => (x -> h y) -> (f :*: g) x -> h ((f :*: g) y)
  traverse1 x_hy (fx :*: gx) =
    liftA2 (:*:) (traverse1 x_hy fx) (traverse1 x_hy gx)
instance
  (Traversable1 f, Traversable1 g) =>
  Traversals1 (->) (->) (f :+: g)
  where
  traverse1 ::
    (Apply h) => (x -> h y) -> (f :+: g) x -> h ((f :+: g) y)
  traverse1 x_hy = \case
    L1 fx -> morphism L1 (traverse1 x_hy fx)
    R1 gx -> morphism R1 (traverse1 x_hy gx)
instance
  (Traversable1 f, Traversable1 g) =>
  Traversals1 (->) (->) (f :.: g)
  where
  traverse1 ::
    forall h x y.
    (Apply h) => (x -> h y) -> (f :.: g) x -> h ((f :.: g) y)
  traverse1 x_hy (Comp1 fgx) = morphism Comp1 do
    traverse1 (traverse1 x_hy :: g x -> h (g y)) fgx

type IxTraversable1 i f =
  (Traversals1 (->) (->) f, Traversals1 (Ix i) (->) f)

instance Traversals1 (Ix ()) (->) Identity where
  traverse1 ::
    (Apply g) => Ix () x (g y) -> Identity x -> g (Identity y)
  traverse1 (Ix u_x_gy) (Identity x) =
    morphism Identity (u_x_gy () x)
instance Traversals1 (Ix Natural) (->) List1 where
  traverse1 ::
    (Apply g) => Ix Natural x (g y) -> List1 x -> g (List1 y)
  traverse1 (Ix i_x_gy) = flip fix 0 \rec i -> \case
    Sole x -> morphism Sole (i_x_gy i x)
    x :|| xs -> liftA2 (:||) (i_x_gy i x) (rec (succ i) xs)
instance Traversals1 (Ix z) (->) ((,) z) where
  traverse1 ::
    (Apply g) => Ix z x (g y) -> (z, x) -> g (z, y)
  traverse1 (Ix z_x_gy) (z, x) =
    morphism (z,) (z_x_gy z x)
instance Traversals1 (Ix (Either () ())) (->) Complex where
  traverse1 ::
    (Apply g) =>
    Ix (Either () ()) x (g y) -> Complex x -> g (Complex y)
  traverse1 (Ix e_x_gy) (xr :+ xi) =
    liftA2 (:+) (e_x_gy (Left ()) xr) (e_x_gy (Right ()) xi)
instance
  (IxTraversable1 i f, IxTraversable1 j g) =>
  Traversals1 (Ix (Either i j)) (->) (Product f g)
  where
  traverse1 ::
    (Apply h) =>
    Ix (Either i j) x (h y) ->
    Product f g x ->
    h (Product f g y)
  traverse1 (Ix eij_x_hy) (Pair fx gx) =
    liftA2
      Pair
      (traverse1 (Ix (eij_x_hy . Left)) fx)
      (traverse1 (Ix (eij_x_hy . Right)) gx)
instance
  (IxTraversable1 i f, IxTraversable1 j g) =>
  Traversals1 (Ix (Either i j)) (->) (Sum f g)
  where
  traverse1 ::
    (Apply h) =>
    Ix (Either i j) x (h y) ->
    Sum f g x ->
    h (Sum f g y)
  traverse1 (Ix eij_x_hy) = \case
    InL fx -> morphism InL (traverse1 (Ix (eij_x_hy . Left)) fx)
    InR gx -> morphism InR (traverse1 (Ix (eij_x_hy . Right)) gx)
instance
  (IxTraversable1 i f, IxTraversable1 j g) =>
  Traversals1 (Ix (i, j)) (->) (Compose f g)
  where
  traverse1 ::
    (Apply h) =>
    Ix (i, j) x (h y) ->
    Compose f g x ->
    h (Compose f g y)
  traverse1 (Ix eij_x_hy) (Compose fgx) = morphism Compose do
    traverse1
      (Ix \i -> traverse1 (Ix \j -> eij_x_hy (i, j)))
      fgx

instance Traversals1 (Ix Void) (->) V1 where
  traverse1 :: (Apply g) => Ix Void x (g y) -> V1 x -> g (V1 y)
  traverse1 _ v = case v of {}
instance Traversals1 (Ix ()) (->) Par1 where
  traverse1 :: (Apply g) => Ix () x (g y) -> Par1 x -> g (Par1 y)
  traverse1 (Ix u_x_gy) (Par1 x) = morphism Par1 (u_x_gy () x)
instance (IxTraversable1 i f) => Traversals1 (Ix i) (->) (Rec1 f) where
  traverse1 :: (Apply g) => Ix i x (g y) -> Rec1 f x -> g (Rec1 f y)
  traverse1 (Ix i_x_gy) (Rec1 fx) = morphism Rec1 (traverse1 (Ix i_x_gy) fx)
instance (IxTraversable1 i f) => Traversals1 (Ix i) (->) (M1 i c f) where
  traverse1 :: (Apply g) => Ix i x (g y) -> M1 i c f x -> g (M1 i c f y)
  traverse1 (Ix i_x_gy) (M1 fx) = morphism M1 (traverse1 (Ix i_x_gy) fx)
instance
  (IxTraversable1 i f, IxTraversable1 j g) =>
  Traversals1 (Ix (Either i j)) (->) (f :*: g)
  where
  traverse1 ::
    (Apply h) =>
    Ix (Either i j) x (h y) ->
    (f :*: g) x ->
    h ((f :*: g) y)
  traverse1 (Ix eij_x_hy) (fx :*: gx) =
    liftA2
      (:*:)
      (traverse1 (Ix (eij_x_hy . Left)) fx)
      (traverse1 (Ix (eij_x_hy . Right)) gx)
instance
  (IxTraversable1 i f, IxTraversable1 j g) =>
  Traversals1 (Ix (Either i j)) (->) (f :+: g)
  where
  traverse1 ::
    (Apply h) =>
    Ix (Either i j) x (h y) ->
    (f :+: g) x ->
    h ((f :+: g) y)
  traverse1 (Ix eij_x_hy) = \case
    L1 fx -> morphism L1 (traverse1 (Ix (eij_x_hy . Left)) fx)
    R1 gx -> morphism R1 (traverse1 (Ix (eij_x_hy . Right)) gx)
instance
  (IxTraversable1 i f, IxTraversable1 j g) =>
  Traversals1 (Ix (i, j)) (->) (f :.: g)
  where
  traverse1 ::
    (Apply h) =>
    Ix (i, j) x (h y) ->
    (f :.: g) x ->
    h ((f :.: g) y)
  traverse1 (Ix eij_x_hy) (Comp1 fgx) = morphism Comp1 do
    traverse1
      (Ix \i -> traverse1 (Ix \j -> eij_x_hy (i, j)))
      fgx

type Bind :: (Type -> Type) -> Constraint
class (Apply f) => Bind f where
  (>>=) :: f x -> (x -> f y) -> f y

(>>) :: (Bind f) => f x -> f y -> f y
fx >> fy = fx >>= const fy

join :: (Bind f) => f (f x) -> f x
join ffx = ffx >>= id

type Monad :: (Type -> Type) -> Constraint
type Monad f = (Pure f, Bind f)

(=<<) :: (Bind f) => (x -> f y) -> f x -> f y
(=<<) = flip (>>=)
(>=>) :: (Bind f) => (x -> f y) -> (y -> f z) -> (x -> f z)
f >=> g = \x -> f x >>= g
(<=<) :: (Bind f) => (y -> f z) -> (x -> f y) -> (x -> f z)
g <=< f = \x -> f x >>= g

liftM2 :: (Monad f) => (x -> y -> z) -> f x -> f y -> f z
liftM2 x_y_z fx fy = fx >>= \ !x -> fy >>= \ !y -> pure (x_y_z x y)

liftM3 :: (Monad f) => (x -> y -> z -> w) -> f x -> f y -> f z -> f w
liftM3 xyzw fx fy fz =
  fx >>= \ !x -> fy >>= \ !y -> fz >>= \ !z -> pure (xyzw x y z)

ap :: (Monad f) => f (x -> y) -> f x -> f y
ap fxy fx = fxy >>= \x_y -> fx >>= \ !x -> pure (x_y x)

void :: (Along f) => f x -> f ()
void = morphism (const ())

when :: (Monad f) => Bool -> f x -> f ()
when b fx = if b then void fx else pure ()

unless :: (Monad f) => Bool -> f x -> f ()
unless b = when (not b)

instance Bind ((->) z) where
  (>>=) :: (z -> x) -> (x -> z -> y) -> z -> y
  z_x >>= x_z_y = \z -> x_z_y (z_x z) z
instance (Bind f) => Bind (Kleisli f z) where
  (>>=) :: Kleisli f z x -> (x -> Kleisli f z y) -> Kleisli f z y
  Kleisli z_fx >>= x_Kzy =
    Kleisli \z -> z_fx z >>= \x -> (x_Kzy x).runKleisli z
instance Bind Identity where
  (>>=) :: Identity x -> (x -> Identity y) -> Identity y
  Identity x >>= f = f x
instance Bind Proxy where
  (>>=) :: Proxy x -> (x -> Proxy y) -> Proxy y
  Proxy >>= _ = Proxy
instance (Semigroup z) => Bind (Const z) where
  (>>=) :: Const z x -> (x -> Const z y) -> Const z y
  Const z >>= _ = Const z
instance Bind Maybe where
  (>>=) :: Maybe x -> (x -> Maybe y) -> Maybe y
  (>>=) = \cases
    Nothing _ -> Nothing
    (Just x) f -> f x
instance Bind (Either l) where
  (>>=) :: Either l x -> (x -> Either l y) -> Either l y
  (>>=) = \cases
    (Left l) _ -> Left l
    (Right x) f -> f x
instance Bind List1 where
  (>>=) :: List1 x -> (x -> List1 y) -> List1 y
  (x :| xs) >>= f = case xs of
    [] -> f x
    (y : ys) -> f x <> (y :| ys) >>= f
instance Bind List where
  (>>=) :: List x -> (x -> List y) -> List y
  (>>=) = \cases
    [] _ -> []
    (x : xs) f -> f x <> (xs >>= f)
instance Bind IO where
  (>>=) :: IO x -> (x -> IO y) -> IO y
  (>>=) = bindIO

instance Bind U1 where
  (>>=) :: U1 x -> (x -> U1 y) -> U1 y
  U1 >>= _ = U1
instance Bind Par1 where
  (>>=) :: Par1 x -> (x -> Par1 y) -> Par1 y
  Par1 x >>= x_Py = x_Py x
instance (Bind f) => Bind (Rec1 f) where
  (>>=) :: Rec1 f x -> (x -> Rec1 f y) -> Rec1 f y
  Rec1 fx >>= x_Rfy = Rec1 (fx >>= \x -> unRec1 (x_Rfy x))
instance (Bind f) => Bind (M1 i c f) where
  (>>=) :: M1 i c f x -> (x -> M1 i c f y) -> M1 i c f y
  M1 fx >>= x_Micfy = M1 (fx >>= \x -> unM1 (x_Micfy x))

class (Along2 f) => Foldable2 f where
  foldWith2 :: (Monoid z) => (x -> z) -> (y -> z) -> f x y -> z

instance Foldable2 Either where
  foldWith2 :: (Monoid z) => (x -> z) -> (y -> z) -> Either x y -> z
  foldWith2 x_z y_z = \case
    Left x -> x_z x
    Right y -> y_z y
instance Foldable2 (,) where
  foldWith2 :: (Monoid z) => (x -> z) -> (y -> z) -> (x, y) -> z
  foldWith2 x_z y_z (x, y) = x_z x <> y_z y
instance Foldable2 Arg where
  foldWith2 :: (Monoid z) => (x -> z) -> (y -> z) -> Arg x y -> z
  foldWith2 x_z y_z (Arg x y) = x_z x <> y_z y
instance Foldable2 Const where
  foldWith2 :: (Monoid z) => (x -> z) -> (y -> z) -> Const x y -> z
  foldWith2 x_z _ (Const x) = x_z x
instance Foldable2 ((,,) a) where
  foldWith2 :: (Monoid z) => (x -> z) -> (y -> z) -> (a, x, y) -> z
  foldWith2 x_z y_z (_, x, y) = x_z x <> y_z y
instance Foldable2 ((,,,) a b) where
  foldWith2 :: (Monoid z) => (x -> z) -> (y -> z) -> (a, b, x, y) -> z
  foldWith2 x_z y_z (_, _, x, y) = x_z x <> y_z y
instance Foldable2 ((,,,,) a b c) where
  foldWith2 :: (Monoid z) => (x -> z) -> (y -> z) -> (a, b, c, x, y) -> z
  foldWith2 x_z y_z (_, _, _, x, y) = x_z x <> y_z y
instance Foldable2 ((,,,,,) a b c d) where
  foldWith2 :: (Monoid z) => (x -> z) -> (y -> z) -> (a, b, c, d, x, y) -> z
  foldWith2 x_z y_z (_, _, _, _, x, y) = x_z x <> y_z y
instance Foldable2 ((,,,,,,) a b c d e) where
  foldWith2 :: (Monoid z) => (x -> z) -> (y -> z) -> (a, b, c, d, e, x, y) -> z
  foldWith2 x_z y_z (_, _, _, _, _, x, y) = x_z x <> y_z y
instance Foldable2 ((,,,,,,,) a b c d e f) where
  foldWith2 :: (Monoid z) => (x -> z) -> (y -> z) -> (a, b, c, d, e, f, x, y) -> z
  foldWith2 x_z y_z (_, _, _, _, _, _, x, y) = x_z x <> y_z y

class (Foldable2 f) => Traversable2 f where
  traverse2 :: (Applicative g) => (x -> g y) -> (z -> g w) -> f x z -> g (f y w)

instance Traversable2 Either where
  traverse2 ::
    (Applicative g) =>
    (x -> g y) -> (z -> g w) -> Either x z -> g (Either y w)
  traverse2 x_gy z_gw = \case
    Left x -> morphism Left (x_gy x)
    Right z -> morphism Right (z_gw z)
instance Traversable2 (,) where
  traverse2 ::
    (Applicative g) =>
    (x -> g y) -> (z -> g w) -> (x, z) -> g (y, w)
  traverse2 x_gy z_gw (x, z) = liftA2 (,) (x_gy x) (z_gw z)
instance Traversable2 Arg where
  traverse2 ::
    (Applicative g) =>
    (x -> g y) -> (z -> g w) -> Arg x z -> g (Arg y w)
  traverse2 x_gy z_gw (Arg x z) = liftA2 Arg (x_gy x) (z_gw z)
instance Traversable2 Const where
  traverse2 ::
    (Applicative g) =>
    (x -> g y) -> (z -> g w) -> Const x z -> g (Const y w)
  traverse2 x_gy _ (Const x) = morphism Const (x_gy x)
instance Traversable2 ((,,) a) where
  traverse2 ::
    (Applicative g) =>
    (x -> g y) -> (z -> g w) -> (a, x, z) -> g (a, y, w)
  traverse2 x_gy z_gw (a, x, z) = liftA2 (a,,) (x_gy x) (z_gw z)
instance Traversable2 ((,,,) a b) where
  traverse2 ::
    (Applicative g) =>
    (x -> g y) -> (z -> g w) -> (a, b, x, z) -> g (a, b, y, w)
  traverse2 x_gy z_gw (a, b, x, z) = liftA2 (a,b,,) (x_gy x) (z_gw z)
instance Traversable2 ((,,,,) a b c) where
  traverse2 ::
    (Applicative g) =>
    (x -> g y) -> (z -> g w) -> (a, b, c, x, z) -> g (a, b, c, y, w)
  traverse2 x_gy z_gw (a, b, c, x, z) =
    liftA2 (a,b,c,,) (x_gy x) (z_gw z)
instance Traversable2 ((,,,,,) a b c d) where
  traverse2 ::
    (Applicative g) =>
    (x -> g y) -> (z -> g w) -> (a, b, c, d, x, z) -> g (a, b, c, d, y, w)
  traverse2 x_gy z_gw (a, b, c, d, x, z) =
    liftA2 (a,b,c,d,,) (x_gy x) (z_gw z)
instance Traversable2 ((,,,,,,) a b c d e) where
  traverse2 ::
    (Applicative g) =>
    (x -> g y) -> (z -> g w) -> (a, b, c, d, e, x, z) -> g (a, b, c, d, e, y, w)
  traverse2 x_gy z_gw (a, b, c, d, e, x, z) =
    liftA2 (a,b,c,d,e,,) (x_gy x) (z_gw z)
instance Traversable2 ((,,,,,,,) a b c d e f) where
  traverse2 ::
    (Applicative g) =>
    (x -> g y) ->
    (z -> g w) ->
    (a, b, c, d, e, f, x, z) ->
    g (a, b, c, d, e, f, y, w)
  traverse2 x_gy z_gw (a, b, c, d, e, f, x, z) =
    liftA2 (a,b,c,d,e,f,,) (x_gy x) (z_gw z)

type Nil :: (Type -> Type) -> Constraint
class Nil f where
  nil :: f x

guard :: (Nil f, Pure f) => Bool -> f ()
guard b = if b then pure () else nil

instance Nil Maybe where
  nil :: Maybe x
  nil = Nothing
instance Nil List where
  nil :: List x
  nil = []
instance (Nil f) => Nil (Kleisli f z) where
  nil :: Kleisli f z x
  nil = Kleisli (const nil)
instance Nil Proxy where
  nil :: Proxy x
  nil = Proxy
instance (Nil f, Nil g) => Nil (Product f g) where
  nil :: Product f g x
  nil = Pair nil nil

instance Nil Seq where
  nil :: Seq x
  nil = Seq.Empty
instance Nil Set where
  nil :: Set x
  nil = Set.empty
instance Nil (Map k) where
  nil :: Map k x
  nil = Map.empty
instance Nil IntMap where
  nil :: IntMap x
  nil = IntMap.empty

instance Nil U1 where
  nil :: U1 x
  nil = U1
instance (Nil f) => Nil (Rec1 f) where
  nil :: Rec1 f x
  nil = Rec1 nil
instance (Nil f) => Nil (M1 i c f) where
  nil :: M1 i c f x
  nil = M1 nil
instance (Nil f, Nil g) => Nil (f :*: g) where
  nil :: (f :*: g) x
  nil = nil :*: nil

type Alt :: (Type -> Type) -> Constraint
class (Applicative f) => Alt f where
  (<|>) :: f x -> f x -> f x
instance Alt Maybe where
  (<|>) :: Maybe x -> Maybe x -> Maybe x
  (<|>) = \cases
    Nothing Nothing -> Nothing
    Nothing (Just x) -> Just x
    (Just x) _ -> Just x
instance Alt List where
  (<|>) :: List x -> List x -> List x
  (<|>) = (<>)
instance (Data.Functor f, Alt f) => Alt (Kleisli f z) where
  (<|>) :: Kleisli f z x -> Kleisli f z x -> Kleisli f z x
  Kleisli x0 <|> Kleisli x1 = Kleisli \z -> x0 z <|> x1 z
instance Alt Proxy where
  (<|>) :: Proxy x -> Proxy x -> Proxy x
  Proxy <|> Proxy = Proxy
instance (Alt f, Alt g) => Alt (Product f g) where
  (<|>) :: Product f g x -> Product f g x -> Product f g x
  Pair fx0 gx0 <|> Pair fx1 gx1 = Pair (fx0 <|> fx1) (gx0 <|> gx1)

instance Alt U1 where
  (<|>) :: U1 x -> U1 x -> U1 x
  U1 <|> U1 = U1
instance (Alt f) => Alt (Rec1 f) where
  (<|>) :: Rec1 f x -> Rec1 f x -> Rec1 f x
  Rec1 fx0 <|> Rec1 fx1 = Rec1 (fx0 <|> fx1)
instance (Alt f) => Alt (M1 i c f) where
  (<|>) :: M1 i c f x -> M1 i c f x -> M1 i c f x
  M1 fx0 <|> M1 fx1 = M1 (fx0 <|> fx1)
instance (Alt f, Alt g) => Alt (f :*: g) where
  (<|>) :: (f :*: g) x -> (f :*: g) x -> (f :*: g) x
  (fx0 :*: gx0) <|> (fx1 :*: gx1) = (fx0 <|> fx1) :*: (gx0 <|> gx1)

asum1 :: (Alt f, Foldable1 t) => t (f x) -> f x
asum1 = foldr1 (<|>)

type Alternative f = (Nil f, Alt f)

asum :: (Alternative f, Foldable t) => t (f x) -> f x
asum = foldr (<|>) nil

type Option :: (Type -> Type) -> Type -> Type
newtype Option f x = Option {getOption :: f x}
  deriving (Data.Functor, Data.Foldable, Data.Traversable)
instance (Along f) => Morphisms (->) (->) (Option f) where
  morphism :: (x -> y) -> Option f x -> Option f y
  morphism x_y (Option fx) = Option (morphism x_y fx)
instance (Against f) => Morphisms Op (->) (Option f) where
  morphism :: Op x y -> Option f x -> Option f y
  morphism (Op y_x) (Option fx) = Option (morphism (Op y_x) fx)
instance (Foldable f) => Folds (->) (->) (Option f) where
  foldWith :: (Monoid z) => (x -> z) -> Option f x -> z
  foldWith x_z (Option fx) = foldWith x_z fx
instance (Foldable1 f) => Folds1 (->) (->) (Option f) where
  foldWith1 :: (Semigroup z) => (x -> z) -> Option f x -> z
  foldWith1 x_z (Option fx) = foldWith1 x_z fx
instance (Traversable f) => Traversals (->) (->) (Option f) where
  traverse :: (Applicative g) => (x -> g y) -> Option f x -> g (Option f y)
  traverse x_gy (Option fx) = morphism Option (traverse x_gy fx)
instance (Traversable1 f) => Traversals1 (->) (->) (Option f) where
  traverse1 :: (Apply g) => (x -> g y) -> Option f x -> g (Option f y)
  traverse1 x_gy (Option fx) = morphism Option (traverse1 x_gy fx)
instance (Pure f) => Pure (Option f) where
  pure :: x -> Option f x
  pure x = Option (pure x)
instance (Apply f) => Apply (Option f) where
  (<*>) :: Option f (x -> y) -> Option f x -> Option f y
  Option x_y <*> Option x = Option (x_y <*> x)
instance (Nil f) => Nil (Option f) where
  nil :: Option f x
  nil = Option nil
instance (Alt f) => Alt (Option f) where
  (<|>) :: Option f x -> Option f x -> Option f x
  Option x0 <|> Option z = Option (x0 <|> z)
instance (Alt f) => Semigroup (Option f x) where
  (<>) :: Option f x -> Option f x -> Option f x
  (<>) = (<|>)
instance (Alternative f) => Monoid (Option f x) where
  mempty :: Option f x
  mempty = nil

type Filterable f = Morphisms (Kleisli Maybe) (->) f

justs :: (Filterable f) => (x -> Maybe y) -> f x -> f y
justs = morphism . Kleisli

filter :: (Filterable f) => (x -> Bool) -> f x -> f x
filter p = morphism (Kleisli \x -> if p x then Just x else Nothing)

instance Morphisms (Kleisli Maybe) (->) Maybe where
  morphism :: Kleisli Maybe x y -> Maybe x -> Maybe y
  morphism (Kleisli x_my) = \case
    Nothing -> Nothing
    Just x -> x_my x
instance Morphisms (Kleisli Maybe) (->) List where
  morphism :: Kleisli Maybe x y -> List x -> List y
  morphism (Kleisli x_my) = \case
    [] -> []
    x : xs -> (<> morphism (Kleisli x_my) xs) case x_my x of
      Nothing -> []
      Just y -> [y]
instance (Monoid z) => Morphisms (Kleisli Maybe) (->) (Either z) where
  morphism :: Kleisli Maybe x y -> Either z x -> Either z y
  morphism (Kleisli x_my) = \case
    Left z -> Left z
    Right x -> case x_my x of
      Nothing -> Left mempty
      Just y -> Right y
instance Morphisms (Kleisli Maybe) (->) Proxy where
  morphism :: Kleisli Maybe x y -> Proxy x -> Proxy y
  morphism (Kleisli _) Proxy = Proxy
instance Morphisms (Kleisli Maybe) (->) (Const z) where
  morphism :: Kleisli Maybe x y -> Const z x -> Const z y
  morphism (Kleisli _) (Const z) = Const z
instance (Filterable f, Filterable g) => Morphisms (Kleisli Maybe) (->) (Product f g) where
  morphism :: Kleisli Maybe x y -> Product f g x -> Product f g y
  morphism (Kleisli x_my) (Pair fx gx) =
    Pair
      (morphism (Kleisli x_my) fx)
      (morphism (Kleisli x_my) gx)
instance (Filterable f, Filterable g) => Morphisms (Kleisli Maybe) (->) (Sum f g) where
  morphism :: Kleisli Maybe x y -> Sum f g x -> Sum f g y
  morphism (Kleisli x_my) = \case
    InL fx -> InL (morphism (Kleisli x_my) fx)
    InR gx -> InR (morphism (Kleisli x_my) gx)
instance (Along f, Filterable g) => Morphisms (Kleisli Maybe) (->) (Compose f g) where
  morphism :: forall x y. Kleisli Maybe x y -> Compose f g x -> Compose f g y
  morphism (Kleisli x_my) (Compose fgx) = Compose do
    morphism
      (morphism (Kleisli x_my) :: g x -> g y)
      fgx

instance Morphisms (Kleisli Maybe) (->) (Map k) where
  morphism :: Kleisli Maybe x y -> Map k x -> Map k y
  morphism (Kleisli x_my) m = Map.mapMaybe x_my m
instance Morphisms (Kleisli Maybe) (->) IntMap where
  morphism :: Kleisli Maybe x y -> IntMap x -> IntMap y
  morphism (Kleisli x_my) m = IntMap.mapMaybe x_my m

instance Morphisms (Kleisli Maybe) (->) V1 where
  morphism :: Kleisli Maybe x y -> V1 x -> V1 y
  morphism (Kleisli _) v = case v of {}
instance Morphisms (Kleisli Maybe) (->) U1 where
  morphism :: Kleisli Maybe x y -> U1 x -> U1 y
  morphism (Kleisli _) U1 = U1
instance (Filterable f) => Morphisms (Kleisli Maybe) (->) (Rec1 f) where
  morphism :: Kleisli Maybe x y -> Rec1 f x -> Rec1 f y
  morphism (Kleisli x_my) (Rec1 fx) = Rec1 (morphism (Kleisli x_my) fx)
instance Morphisms (Kleisli Maybe) (->) (K1 i c) where
  morphism :: Kleisli Maybe x y -> K1 i c x -> K1 i c y
  morphism (Kleisli _) (K1 c) = K1 c
instance (Filterable f) => Morphisms (Kleisli Maybe) (->) (M1 i c f) where
  morphism :: Kleisli Maybe x y -> M1 i c f x -> M1 i c f y
  morphism (Kleisli x_my) (M1 fx) = M1 (morphism (Kleisli x_my) fx)
instance (Filterable f, Filterable g) => Morphisms (Kleisli Maybe) (->) (f :*: g) where
  morphism :: Kleisli Maybe x y -> (f :*: g) x -> (f :*: g) y
  morphism (Kleisli x_my) (fx :*: gx) =
    morphism (Kleisli x_my) fx :*: morphism (Kleisli x_my) gx
instance (Filterable f, Filterable g) => Morphisms (Kleisli Maybe) (->) (f :+: g) where
  morphism :: Kleisli Maybe x y -> (f :+: g) x -> (f :+: g) y
  morphism (Kleisli x_my) = \case
    L1 fx -> L1 (morphism (Kleisli x_my) fx)
    R1 gx -> R1 (morphism (Kleisli x_my) gx)
instance (Along f, Filterable g) => Morphisms (Kleisli Maybe) (->) (f :.: g) where
  morphism :: forall x y. Kleisli Maybe x y -> (f :.: g) x -> (f :.: g) y
  morphism (Kleisli x_my) (Comp1 fgx) = Comp1 do
    morphism
      (morphism (Kleisli x_my) :: g x -> g y)
      fgx

type IxFilterable i f =
  ( Morphisms (Kleisli Maybe) (->) f
  , Morphisms (Procompose (Kleisli Maybe) (Ix i)) (->) f
  )

ijusts :: (IxFilterable i f) => (i -> x -> Maybe y) -> f x -> f y
ijusts i_x_my = morphism (Procompose (Kleisli id) (Ix i_x_my))

ifilter :: (IxFilterable i f) => (i -> x -> Bool) -> f x -> f x
ifilter i_x_b =
  morphism
    ( Procompose
        (Kleisli \(b, x) -> if b then Just x else Nothing)
        (Ix \i x -> (i_x_b i x, x))
    )

instance Morphisms (Procompose (Kleisli Maybe) (Ix ())) (->) Maybe where
  morphism :: Procompose (Kleisli Maybe) (Ix ()) x y -> Maybe x -> Maybe y
  morphism (Procompose (Kleisli z_my) (Ix u_x_z)) = \case
    Nothing -> Nothing
    Just x -> z_my (u_x_z () x)
instance Morphisms (Procompose (Kleisli Maybe) (Ix Int)) (->) List where
  morphism :: Procompose (Kleisli Maybe) (Ix Int) x y -> List x -> List y
  morphism (Procompose (Kleisli z_my) (Ix i_x_z)) = flip fix 0 \rec i -> \case
    [] -> []
    x : xs -> (<> rec (succ i) xs) case z_my (i_x_z i x) of
      Nothing -> []
      Just y -> [y]
instance Morphisms (Procompose (Kleisli Maybe) (Ix Integer)) (->) List where
  morphism :: Procompose (Kleisli Maybe) (Ix Integer) x y -> List x -> List y
  morphism (Procompose (Kleisli z_my) (Ix i_x_z)) = flip fix 0 \rec i -> \case
    [] -> []
    x : xs -> (<> rec (succ i) xs) case z_my (i_x_z i x) of
      Nothing -> []
      Just y -> [y]
instance Morphisms (Procompose (Kleisli Maybe) (Ix Natural)) (->) List where
  morphism :: Procompose (Kleisli Maybe) (Ix Natural) x y -> List x -> List y
  morphism (Procompose (Kleisli z_my) (Ix i_x_z)) = flip fix 0 \rec i -> \case
    [] -> []
    x : xs -> (<> rec (succ i) xs) case z_my (i_x_z i x) of
      Nothing -> []
      Just y -> [y]
instance (Monoid z) => Morphisms (Procompose (Kleisli Maybe) (Ix ())) (->) (Either z) where
  morphism :: Procompose (Kleisli Maybe) (Ix ()) x y -> Either z x -> Either z y
  morphism (Procompose (Kleisli z_my) (Ix u_x_z)) = \case
    Left z -> Left z
    Right x -> case z_my (u_x_z () x) of
      Nothing -> Left mempty
      Just y -> Right y
instance Morphisms (Procompose (Kleisli Maybe) (Ix Void)) (->) Proxy where
  morphism :: Procompose (Kleisli Maybe) (Ix Void) x y -> Proxy x -> Proxy y
  morphism (Procompose (Kleisli _) (Ix _)) Proxy = Proxy
instance Morphisms (Procompose (Kleisli Maybe) (Ix Void)) (->) (Const z) where
  morphism :: Procompose (Kleisli Maybe) (Ix Void) x y -> Const z x -> Const z y
  morphism (Procompose (Kleisli _) (Ix _)) (Const z) = Const z
instance
  (IxFilterable i f, IxFilterable j g) =>
  Morphisms (Procompose (Kleisli Maybe) (Ix (Either i j))) (->) (Product f g)
  where
  morphism ::
    Procompose (Kleisli Maybe) (Ix (Either i j)) x y ->
    Product f g x ->
    Product f g y
  morphism (Procompose (Kleisli z_my) (Ix eij_x_z)) (Pair fx gx) =
    Pair
      ( morphism
          (Procompose (Kleisli z_my) (Ix (eij_x_z . Left)))
          fx
      )
      ( morphism
          (Procompose (Kleisli z_my) (Ix (eij_x_z . Right)))
          gx
      )
instance
  (IxFilterable i f, IxFilterable j g) =>
  Morphisms (Procompose (Kleisli Maybe) (Ix (Either i j))) (->) (Sum f g)
  where
  morphism ::
    Procompose (Kleisli Maybe) (Ix (Either i j)) x y ->
    Sum f g x ->
    Sum f g y
  morphism (Procompose (Kleisli z_my) (Ix eij_x_z)) = \case
    InL fx -> InL (morphism (Procompose (Kleisli z_my) (Ix (eij_x_z . Left))) fx)
    InR gx -> InR (morphism (Procompose (Kleisli z_my) (Ix (eij_x_z . Right))) gx)
instance
  (IxAlong i f, IxFilterable j g) =>
  Morphisms (Procompose (Kleisli Maybe) (Ix (i, j))) (->) (Compose f g)
  where
  morphism ::
    Procompose (Kleisli Maybe) (Ix (i, j)) x y ->
    Compose f g x ->
    Compose f g y
  morphism (Procompose (Kleisli z_my) (Ix ij_x_z)) (Compose fgx) = Compose do
    morphism
      (Ix \i -> morphism (Procompose (Kleisli z_my) (Ix (ij_x_z . (i,)))))
      fgx

instance Morphisms (Procompose (Kleisli Maybe) (Ix k)) (->) (Map k) where
  morphism :: Procompose (Kleisli Maybe) (Ix k) x y -> Map k x -> Map k y
  morphism (Procompose (Kleisli z_my) (Ix k_x_z)) m = Map.mapMaybe z_my (Map.mapWithKey k_x_z m)
instance Morphisms (Procompose (Kleisli Maybe) (Ix Int)) (->) IntMap where
  morphism :: Procompose (Kleisli Maybe) (Ix Int) x y -> IntMap x -> IntMap y
  morphism (Procompose (Kleisli z_my) (Ix i_x_z)) m = IntMap.mapMaybe z_my (IntMap.mapWithKey i_x_z m)

instance Morphisms (Procompose (Kleisli Maybe) (Ix Void)) (->) V1 where
  morphism :: Procompose (Kleisli Maybe) (Ix Void) x y -> V1 x -> V1 y
  morphism (Procompose (Kleisli _) (Ix _)) v = case v of {}
instance Morphisms (Procompose (Kleisli Maybe) (Ix Void)) (->) U1 where
  morphism :: Procompose (Kleisli Maybe) (Ix Void) x y -> U1 x -> U1 y
  morphism (Procompose (Kleisli _) (Ix _)) U1 = U1
instance
  (IxFilterable i f) =>
  Morphisms (Procompose (Kleisli Maybe) (Ix i)) (->) (Rec1 f)
  where
  morphism :: Procompose (Kleisli Maybe) (Ix i) x y -> Rec1 f x -> Rec1 f y
  morphism p (Rec1 fx) = Rec1 (morphism p fx)
instance Morphisms (Procompose (Kleisli Maybe) (Ix Void)) (->) (K1 i c) where
  morphism :: Procompose (Kleisli Maybe) (Ix Void) x y -> K1 i c x -> K1 i c y
  morphism (Procompose (Kleisli _) (Ix _)) (K1 c) = K1 c
instance
  (IxFilterable i f) =>
  Morphisms (Procompose (Kleisli Maybe) (Ix i)) (->) (M1 i c f)
  where
  morphism :: Procompose (Kleisli Maybe) (Ix i) x y -> M1 i c f x -> M1 i c f y
  morphism p (M1 fx) = M1 (morphism p fx)
instance
  (IxFilterable i f, IxFilterable j g) =>
  Morphisms (Procompose (Kleisli Maybe) (Ix (Either i j))) (->) (f :*: g)
  where
  morphism ::
    Procompose (Kleisli Maybe) (Ix (Either i j)) x y ->
    (f :*: g) x ->
    (f :*: g) y
  morphism (Procompose (Kleisli z_my) (Ix eij_x_z)) (fx :*: gx) =
    morphism
      (Procompose (Kleisli z_my) (Ix (eij_x_z . Left)))
      fx
      :*: morphism
        (Procompose (Kleisli z_my) (Ix (eij_x_z . Right)))
        gx
instance
  (IxFilterable i f, IxFilterable j g) =>
  Morphisms (Procompose (Kleisli Maybe) (Ix (Either i j))) (->) (f :+: g)
  where
  morphism ::
    Procompose (Kleisli Maybe) (Ix (Either i j)) x y ->
    (f :+: g) x ->
    (f :+: g) y
  morphism (Procompose (Kleisli z_my) (Ix eij_x_z)) = \case
    L1 fx -> L1 (morphism (Procompose (Kleisli z_my) (Ix (eij_x_z . Left))) fx)
    R1 gx -> R1 (morphism (Procompose (Kleisli z_my) (Ix (eij_x_z . Right))) gx)
instance
  (IxAlong i f, IxFilterable j g) =>
  Morphisms (Procompose (Kleisli Maybe) (Ix (i, j))) (->) (f :.: g)
  where
  morphism ::
    Procompose (Kleisli Maybe) (Ix (i, j)) x y ->
    (f :.: g) x ->
    (f :.: g) y
  morphism (Procompose (Kleisli z_my) (Ix ij_x_z)) (Comp1 fgx) = Comp1 do
    morphism
      (Ix \i -> morphism (Procompose (Kleisli z_my) (Ix (ij_x_z . (i,)))))
      fgx

class (Traversable f, Filterable f) => Witherable f where
  witherK ::
    (Applicative g) => Kleisli (Compose g Maybe) x y -> f x -> g (f y)

wither :: (Witherable f, Applicative g) => (x -> g (Maybe y)) -> f x -> g (f y)
wither = witherK . Kleisli . (Compose .)

filterA :: (Witherable f, Applicative g) => (x -> g Bool) -> f x -> g (f x)
filterA p =
  witherK (Kleisli \x -> Compose (morphism (\b -> if b then pure x else nil) (p x)))

instance Witherable Maybe where
  witherK ::
    (Applicative g) =>
    Kleisli (Compose g Maybe) x y -> Maybe x -> g (Maybe y)
  witherK (Kleisli x_Cgmy) = \case
    Nothing -> pure Nothing
    Just x -> getCompose (x_Cgmy x)
instance Witherable List where
  witherK ::
    (Applicative g) =>
    Kleisli (Compose g Maybe) x y -> List x -> g (List y)
  witherK (Kleisli x_Cgmy) =
    foldr (liftA2 (maybe id (:)) . getCompose . x_Cgmy) (pure [])
instance Witherable Proxy where
  witherK ::
    (Applicative g) =>
    Kleisli (Compose g Maybe) x y -> Proxy x -> g (Proxy y)
  witherK (Kleisli _) Proxy = pure Proxy
instance Witherable (Const z) where
  witherK ::
    (Applicative g) =>
    Kleisli (Compose g Maybe) x y -> Const z x -> g (Const z y)
  witherK (Kleisli _) (Const z) = pure (Const z)
instance (Witherable f, Witherable g) => Witherable (Product f g) where
  witherK ::
    (Applicative h) =>
    Kleisli (Compose h Maybe) x y -> Product f g x -> h (Product f g y)
  witherK (Kleisli x_hmy) (Pair fx gx) =
    liftA2
      Pair
      (witherK (Kleisli x_hmy) fx)
      (witherK (Kleisli x_hmy) gx)
instance (Witherable f, Witherable g) => Witherable (Sum f g) where
  witherK ::
    (Applicative h) =>
    Kleisli (Compose h Maybe) x y -> Sum f g x -> h (Sum f g y)
  witherK (Kleisli x_hmy) = \case
    InL fx -> morphism InL (witherK (Kleisli x_hmy) fx)
    InR gx -> morphism InR (witherK (Kleisli x_hmy) gx)
instance (Traversable f, Witherable g) => Witherable (Compose f g) where
  witherK ::
    (Applicative h) =>
    Kleisli (Compose h Maybe) x y -> Compose f g x -> h (Compose f g y)
  witherK (Kleisli x_hmy) (Compose fgx) =
    morphism Compose (traverse (witherK (Kleisli x_hmy)) fgx)

instance Witherable V1 where
  witherK ::
    (Applicative g) =>
    Kleisli (Compose g Maybe) x y -> V1 x -> g (V1 y)
  witherK (Kleisli _) v = case v of {}
instance Witherable U1 where
  witherK ::
    (Applicative g) =>
    Kleisli (Compose g Maybe) x y -> U1 x -> g (U1 y)
  witherK (Kleisli _) U1 = pure U1
instance (Witherable f) => Witherable (Rec1 f) where
  witherK ::
    (Applicative g) =>
    Kleisli (Compose g Maybe) x y -> Rec1 f x -> g (Rec1 f y)
  witherK (Kleisli x_gmy) (Rec1 fx) = morphism Rec1 (witherK (Kleisli x_gmy) fx)
instance Witherable (K1 i c) where
  witherK ::
    (Applicative g) =>
    Kleisli (Compose g Maybe) x y -> K1 i c x -> g (K1 i c y)
  witherK (Kleisli _) (K1 c) = pure (K1 c)
instance (Witherable f) => Witherable (M1 i c f) where
  witherK ::
    (Applicative g) =>
    Kleisli (Compose g Maybe) x y -> M1 i c f x -> g (M1 i c f y)
  witherK (Kleisli x_gmy) (M1 fx) = morphism M1 (witherK (Kleisli x_gmy) fx)
instance (Witherable f, Witherable g) => Witherable (f :*: g) where
  witherK ::
    (Applicative h) =>
    Kleisli (Compose h Maybe) x y -> (f :*: g) x -> h ((f :*: g) y)
  witherK (Kleisli x_hmy) (fx :*: gx) =
    liftA2
      (:*:)
      (witherK (Kleisli x_hmy) fx)
      (witherK (Kleisli x_hmy) gx)
instance (Witherable f, Witherable g) => Witherable (f :+: g) where
  witherK ::
    (Applicative h) =>
    Kleisli (Compose h Maybe) x y -> (f :+: g) x -> h ((f :+: g) y)
  witherK (Kleisli x_hmy) = \case
    L1 fx -> morphism L1 (witherK (Kleisli x_hmy) fx)
    R1 gx -> morphism R1 (witherK (Kleisli x_hmy) gx)
instance (Traversable f, Witherable g) => Witherable (f :.: g) where
  witherK ::
    (Applicative h) =>
    Kleisli (Compose h Maybe) x y -> (f :.: g) x -> h ((f :.: g) y)
  witherK (Kleisli x_hmy) (Comp1 fgx) =
    morphism Comp1 (traverse (witherK (Kleisli x_hmy)) fgx)

traverseMaybeWithKeyMap :: (Applicative f) => (k -> x -> f (Maybe y)) -> Map k x -> f (Map k y)
traverseMaybeWithKeyMap = go
 where
  go _ Map.Tip = pure Map.Tip
  go f (Map.Bin _ kx x Map.Tip Map.Tip) = morphism (maybe Map.Tip (\ !x' -> Map.Bin 1 kx x' Map.Tip Map.Tip)) (f kx x)
  go f (Map.Bin _ kx x l r) = liftA3 combine (go f l) (f kx x) (go f r)
    where
      combine !l' mx !r' = case mx of
        Nothing -> Map.link2 l' r'
        Just !x' -> Map.link kx x' l' r'

traverseMaybeWithKeyIntMap :: (Applicative f) => (Int -> x -> f (Maybe y)) -> IntMap x -> f (IntMap y)
traverseMaybeWithKeyIntMap f = go
 where
  go IntMap.Nil = pure IntMap.Nil
  go (IntMap.Tip k x) = morphism (maybe IntMap.Nil (IntMap.Tip k)) (f k x)
  go (IntMap.Bin p l r)
    | Internal.signBranch p = liftA2 (flip (IntMap.bin p)) (go r) (go l)
    | otherwise = liftA2 (IntMap.bin p) (go l) (go r)

instance Witherable (Map k) where
  witherK :: (Applicative g) => Kleisli (Compose g Maybe) x y -> Map k x -> g (Map k y)
  witherK (Kleisli x_gmy) = traverseMaybeWithKeyMap (const (getCompose . x_gmy))
instance Witherable IntMap where
  witherK :: Applicative g => Kleisli (Compose g Maybe) x y -> IntMap x -> g (IntMap y)
  witherK (Kleisli x_gmy) = traverseMaybeWithKeyIntMap (const (getCompose . x_gmy))

class (IxFilterable i f, Witherable f) => IxWitherable i f where
  iwitherK ::
    (Applicative g) =>
    Procompose (Kleisli (Compose g Maybe)) (Ix i) x y -> f x -> g (f y)

iwither ::
  (IxWitherable i f, Applicative g) => (i -> x -> g (Maybe y)) -> f x -> g (f y)
iwither i_x_gmy = iwitherK (Procompose (Kleisli Compose) (Ix i_x_gmy))

ifilterA ::
  (IxWitherable i f, Applicative g) => (i -> x -> g Bool) -> f x -> g (f x)
ifilterA i_x_gb =
  iwitherK
    ( Procompose
        (Kleisli Compose)
        (Ix \i x -> morphism (\b -> if b then pure x else nil) (i_x_gb i x))
    )

instance IxWitherable () Maybe where
  iwitherK ::
    (Applicative g) =>
    Procompose (Kleisli (Compose g Maybe)) (Ix ()) x y ->
    Maybe x ->
    g (Maybe y)
  iwitherK (Procompose (Kleisli z_Cgmy) (Ix u_x_z)) = \case
    Nothing -> pure Nothing
    Just x -> getCompose (z_Cgmy (u_x_z () x))
instance IxWitherable Natural List where
  iwitherK ::
    (Applicative g) =>
    Procompose (Kleisli (Compose g Maybe)) (Ix Natural) x y ->
    List x ->
    g (List y)
  iwitherK (Procompose (Kleisli z_Cgmy) (Ix i_x_z)) =
    ifoldr
      (\i -> liftA2 (maybe id (:)) . getCompose . z_Cgmy . i_x_z i)
      (pure [])
instance IxWitherable Void Proxy where
  iwitherK ::
    (Applicative g) =>
    Procompose (Kleisli (Compose g Maybe)) (Ix Void) x y ->
    Proxy x ->
    g (Proxy y)
  iwitherK (Procompose (Kleisli _) (Ix _)) Proxy = pure Proxy
instance IxWitherable Void (Const z) where
  iwitherK ::
    (Applicative g) =>
    Procompose (Kleisli (Compose g Maybe)) (Ix Void) x y ->
    Const z x ->
    g (Const z y)
  iwitherK (Procompose (Kleisli _) (Ix _)) (Const z) = pure (Const z)
instance (IxWitherable i f, IxWitherable j g) => IxWitherable (Either i j) (Product f g) where
  iwitherK ::
    (Applicative h) =>
    Procompose (Kleisli (Compose h Maybe)) (Ix (Either i j)) x y ->
    Product f g x ->
    h (Product f g y)
  iwitherK (Procompose (Kleisli z_hmy) (Ix eij_x_z)) (Pair fx gx) =
    liftA2
      Pair
      (iwitherK (Procompose (Kleisli z_hmy) (Ix (eij_x_z . Left))) fx)
      (iwitherK (Procompose (Kleisli z_hmy) (Ix (eij_x_z . Right))) gx)
instance (IxWitherable i f, IxWitherable j g) => IxWitherable (Either i j) (Sum f g) where
  iwitherK ::
    (Applicative h) =>
    Procompose (Kleisli (Compose h Maybe)) (Ix (Either i j)) x y ->
    Sum f g x ->
    h (Sum f g y)
  iwitherK (Procompose (Kleisli z_hmy) (Ix eij_x_z)) = \case
    InL fx ->
      morphism InL (iwitherK (Procompose (Kleisli z_hmy) (Ix (eij_x_z . Left))) fx)
    InR gx ->
      morphism InR (iwitherK (Procompose (Kleisli z_hmy) (Ix (eij_x_z . Right))) gx)
instance (IxTraversable i f, IxWitherable j g) => IxWitherable (i, j) (Compose f g) where
  iwitherK ::
    (Applicative h) =>
    Procompose (Kleisli (Compose h Maybe)) (Ix (i, j)) x y ->
    Compose f g x ->
    h (Compose f g y)
  iwitherK (Procompose (Kleisli z_hmy) (Ix ij_x_z)) (Compose fgx) = morphism Compose do
    traverse
      (Ix \i -> iwitherK (Procompose (Kleisli z_hmy) (Ix (ij_x_z . (i,)))))
      fgx

instance IxWitherable Void V1 where
  iwitherK ::
    (Applicative g) =>
    Procompose (Kleisli (Compose g Maybe)) (Ix Void) x y -> V1 x -> g (V1 y)
  iwitherK (Procompose (Kleisli _) (Ix _)) v = case v of {}
instance IxWitherable Void U1 where
  iwitherK ::
    (Applicative g) =>
    Procompose (Kleisli (Compose g Maybe)) (Ix Void) x y -> U1 x -> g (U1 y)
  iwitherK (Procompose (Kleisli _) (Ix _)) U1 = pure U1
instance (IxWitherable i f) => IxWitherable i (Rec1 f) where
  iwitherK ::
    (Applicative g) =>
    Procompose (Kleisli (Compose g Maybe)) (Ix i) x y ->
    Rec1 f x ->
    g (Rec1 f y)
  iwitherK p (Rec1 fx) = morphism Rec1 (iwitherK p fx)
instance IxWitherable Void (K1 i c) where
  iwitherK ::
    (Applicative g) =>
    Procompose (Kleisli (Compose g Maybe)) (Ix Void) x y ->
    K1 i c x ->
    g (K1 i c y)
  iwitherK (Procompose (Kleisli _) (Ix _)) (K1 c) = pure (K1 c)
instance (IxWitherable i f) => IxWitherable i (M1 i c f) where
  iwitherK ::
    (Applicative g) =>
    Procompose (Kleisli (Compose g Maybe)) (Ix i) x y ->
    M1 i c f x ->
    g (M1 i c f y)
  iwitherK p (M1 fx) = morphism M1 (iwitherK p fx)
instance (IxWitherable i f, IxWitherable j g) => IxWitherable (Either i j) (f :*: g) where
  iwitherK ::
    (Applicative h) =>
    Procompose (Kleisli (Compose h Maybe)) (Ix (Either i j)) x y ->
    (f :*: g) x ->
    h ((f :*: g) y)
  iwitherK (Procompose (Kleisli z_hmy) (Ix eij_x_z)) (fx :*: gx) =
    liftA2
      (:*:)
      (iwitherK (Procompose (Kleisli z_hmy) (Ix (eij_x_z . Left))) fx)
      (iwitherK (Procompose (Kleisli z_hmy) (Ix (eij_x_z . Right))) gx)
instance (IxWitherable i f, IxWitherable j g) => IxWitherable (Either i j) (f :+: g) where
  iwitherK ::
    (Applicative h) =>
    Procompose (Kleisli (Compose h Maybe)) (Ix (Either i j)) x y ->
    (f :+: g) x ->
    h ((f :+: g) y)
  iwitherK (Procompose (Kleisli z_hmy) (Ix eij_x_z)) = \case
    L1 fx ->
      morphism L1 (iwitherK (Procompose (Kleisli z_hmy) (Ix (eij_x_z . Left))) fx)
    R1 gx ->
      morphism R1 (iwitherK (Procompose (Kleisli z_hmy) (Ix (eij_x_z . Right))) gx)
instance (IxTraversable i f, IxWitherable j g) => IxWitherable (i, j) (f :.: g) where
  iwitherK ::
    (Applicative h) =>
    Procompose (Kleisli (Compose h Maybe)) (Ix (i, j)) x y ->
    (f :.: g) x ->
    h ((f :.: g) y)
  iwitherK (Procompose (Kleisli z_hmy) (Ix ij_x_z)) (Comp1 fgx) = morphism Comp1 do
    traverse
      (Ix \i -> iwitherK (Procompose (Kleisli z_hmy) (Ix (ij_x_z . (i,)))))
      fgx

instance IxWitherable k (Map k) where
  iwitherK ::
    (Applicative g) =>
    Procompose (Kleisli (Compose g Maybe)) (Ix k) x y ->
    Map k x ->
    g (Map k y)
  iwitherK (Procompose (Kleisli z_gmy) (Ix k_x_z)) =
    traverseMaybeWithKeyMap (\k x -> getCompose (z_gmy (k_x_z k x)))
instance IxWitherable Int IntMap where
  iwitherK ::
    (Applicative g) =>
    Procompose (Kleisli (Compose g Maybe)) (Ix Int) x y ->
    IntMap x ->
    g (IntMap y)
  iwitherK (Procompose (Kleisli z_gmy) (Ix i_x_z)) =
    traverseMaybeWithKeyIntMap (\i x -> getCompose (z_gmy (i_x_z i x)))

-- Comonads

type Copure :: (Type -> Type) -> Constraint
class Copure f where
  copure :: f x -> x

instance Copure Identity where
  copure :: Identity x -> x
  copure = runIdentity
instance Copure List1 where
  copure :: List1 x -> x
  copure = List1.head
instance Copure ((,) z) where
  copure :: (z, x) -> x
  copure = snd
instance (Monoid z) => Copure ((->) z) where
  copure :: (Monoid z) => (z -> x) -> x
  copure = ($ mempty)
instance Copure (Arg z) where
  copure :: Arg z x -> x
  copure (Arg _ x) = x

type Extend :: (Type -> Type) -> Constraint
class (Along f) => Extend f where
  {-# MINIMAL duplicate | extend #-}
  duplicate :: f x -> f (f x)
  duplicate = extend id
  extend :: (f x -> y) -> f x -> f y
  extend f = morphism f . duplicate

type Comonad f = (Copure f, Extend f)

instance Extend Identity where
  duplicate :: Identity x -> Identity (Identity x)
  duplicate = Identity
  extend :: (Identity x -> y) -> Identity x -> Identity y
  extend = (Identity .)
instance Extend List1 where
  extend :: (List1 x -> y) -> List1 x -> List1 y
  extend f w@(~(_ :| xs0)) =
    f w :| case xs0 of
      [] -> []
      x : xs -> List1.toList (extend f (x :| xs))
instance Extend ((,) z) where
  duplicate :: (z, x) -> (z, (z, x))
  duplicate (z, x) = (z, (z, x))
  extend :: ((z, x) -> y) -> (z, x) -> (z, y)
  extend zx_y (z, x) = (z, zx_y (z, x))
instance (Semigroup z) => Extend ((->) z) where
  duplicate :: (z -> x) -> z -> (z -> x)
  duplicate f z z' = f (z <> z')
  extend :: ((z -> x) -> y) -> (z -> x) -> z -> y
  extend f = morphism f . duplicate
instance Extend (Arg z) where
  duplicate :: Arg z x -> Arg z (Arg z x)
  duplicate (Arg z x) = Arg z (Arg z x)
  extend :: (Arg z x -> y) -> Arg z x -> Arg z y
  extend f (Arg z x) = Arg z (f (Arg z x))

type Coapply :: (Type -> Type) -> Constraint
class (Along f, Copure f) => Coapply f where
  (<@>) :: f (x -> y) -> f x -> f y
  default (<@>) :: (Apply f) => f (x -> y) -> f x -> f y
  (<@>) = (<*>)

instance Coapply Identity
instance Coapply List1 where
  (<@>) :: List1 (x -> y) -> List1 x -> List1 y
  (<@>) = Control.ap
instance (Semigroup z) => Coapply ((,) z) where
  (<@>) :: (Semigroup z) => (z, x -> y) -> (z, x) -> (z, y)
  (z0, x_y) <@> (z1, x) = (z0 <> z1, x_y x)
instance (Monoid z) => Coapply ((->) z)

type Cokleisli :: (Type -> Type) -> Type -> Type -> Type
newtype Cokleisli f z x = Cokleisli {runCokleisli :: f z -> x}

instance Data.Functor (Cokleisli f z) where
  fmap :: (x -> y) -> Cokleisli f z x -> Cokleisli f z y
  fmap x_y (Cokleisli fz_x) = Cokleisli (x_y . fz_x)
instance Morphisms (->) (->) (Cokleisli f z) where
  morphism :: (x -> y) -> Cokleisli f z x -> Cokleisli f z y
  morphism x_y (Cokleisli fz_x) = Cokleisli (x_y . fz_x)
instance Apply (Cokleisli f z) where
  (<*>) ::
    Cokleisli f z (x -> y) -> Cokleisli f z x -> Cokleisli f z y
  Cokleisli fz_x_y <*> Cokleisli fz_x =
    Cokleisli \fz -> fz_x_y fz (fz_x fz)
instance Bind (Cokleisli f z) where
  (>>=) ::
    Cokleisli f z x -> (x -> Cokleisli f z y) -> Cokleisli f z y
  Cokleisli fz_x >>= x_Cfzy =
    Cokleisli \fz -> (x_Cfzy (fz_x fz)).runCokleisli fz
instance Control.Applicative (Cokleisli f z) where
  pure :: x -> Cokleisli f z x
  pure x = Cokleisli (const x)
  (<*>) ::
    Cokleisli f z (x -> y) -> Cokleisli f z x -> Cokleisli f z y
  Cokleisli fz_x_y <*> Cokleisli fz_x =
    Cokleisli \fz -> fz_x_y fz (fz_x fz)
instance Control.Monad (Cokleisli f z) where
  (>>=) ::
    Cokleisli f z x -> (x -> Cokleisli f z y) -> Cokleisli f z y
  Cokleisli fz_x >>= x_Cfzy =
    Cokleisli \fz -> (x_Cfzy (fz_x fz)).runCokleisli fz
instance (Comonad f) => Category (Cokleisli f) where
  type Objects (Cokleisli f) = C0
  id :: Cokleisli f x x
  id = Cokleisli copure
  (.) :: Cokleisli f y z -> Cokleisli f x y -> Cokleisli f x z
  Cokleisli fy_z . Cokleisli fx_y = Cokleisli (fy_z . extend fx_y)

type StoreT :: Type -> (Type -> Type) -> Type -> Type
newtype StoreT s f x = StoreT {runStoreT :: (f (s -> x), s)}
type Store s = StoreT s Identity

store :: (s -> x) -> s -> Store s x
store s_x s = StoreT (Identity s_x, s)
runStore :: Store s x -> (s -> x, s)
runStore = morphism' runIdentity . runStoreT

instance (Data.Functor f) => Data.Functor (StoreT s f) where
  fmap :: (x -> y) -> StoreT s f x -> StoreT s f y
  fmap x_y (StoreT (f_s_x, s)) = StoreT (Data.fmap (x_y .) f_s_x, s)
instance (Along f) => Morphisms (->) (->) (StoreT s f) where
  morphism :: (x -> y) -> StoreT s f x -> StoreT s f y
  morphism x_y (StoreT (f_s_x, s)) =
    StoreT (morphism (x_y .) f_s_x, s)
instance (Pure f, Monoid s) => Pure (StoreT s f) where
  pure :: x -> StoreT s f x
  pure x = StoreT (pure (const x), mempty)
instance (Apply f, Semigroup s) => Apply (StoreT s f) where
  (<*>) :: StoreT s f (x -> y) -> StoreT s f x -> StoreT s f y
  StoreT (f_s_x_y, s0) <*> StoreT (f_s_x, s1) =
    StoreT (liftA2 (<*>) f_s_x_y f_s_x, s0 <> s1)
instance
  (Control.Applicative f, Monoid s) =>
  Control.Applicative (StoreT s f)
  where
  pure :: a -> StoreT s f a
  pure x = StoreT (Control.pure (const x), mempty)
  (<*>) :: StoreT s f (x -> y) -> StoreT s f x -> StoreT s f y
  StoreT (f_s_x_y, s0) <*> StoreT (f_s_x, s1) =
    StoreT (Control.liftA2 (Control.<*>) f_s_x_y f_s_x, s0 <> s1)
instance (Copure f) => Copure (StoreT s f) where
  copure :: StoreT s f x -> x
  copure (StoreT (f_s_x, s)) = copure f_s_x s
instance (Extend f) => Extend (StoreT s f) where
  duplicate :: StoreT s f x -> StoreT s f (StoreT s f x)
  duplicate (StoreT (f_s_x, s)) =
    StoreT (extend (curry StoreT) f_s_x, s)
  extend :: (StoreT s f x -> y) -> StoreT s f x -> StoreT s f y
  extend ssfx_y (StoreT (f_s_x, s)) =
    StoreT (flip extend f_s_x \wf s' -> ssfx_y (StoreT (wf, s')), s)
instance (Coapply f, Semigroup s) => Coapply (StoreT s f) where
  (<@>) :: StoreT s f (x -> y) -> StoreT s f x -> StoreT s f y
  StoreT (s_f_x_y, s0) <@> StoreT (s_f_x, s1) =
    StoreT (morphism (<*>) s_f_x_y <@> s_f_x, s0 <> s1)

class (Comonad f) => ComonadStore s f | f -> s where
  {-# MINIMAL position, peek #-}
  position :: f x -> s
  peek :: s -> f x -> x

  peeks :: (s -> s) -> f x -> x
  peeks s_s fx = peek (s_s (position fx)) fx

  seek :: s -> f x -> f x
  seek s = peek s . duplicate

  seeks :: (s -> s) -> f x -> f x
  seeks f = peeks f . duplicate

  experiment :: (Along g) => (s -> g s) -> f x -> g x
  experiment s_gs fx = morphism (`peek` fx) (s_gs (position fx))

instance (Comonad f) => ComonadStore s (StoreT s f) where
  position :: StoreT s f x -> s
  position (StoreT (_, s)) = s
  peek :: s -> StoreT s f x -> x
  peek s (StoreT (f_s_x, _)) = copure f_s_x s

-- Collectable and Tabulation

type Collectable :: (Type -> Type) -> Constraint
class (Along d) => Collectable d where
  {-# MINIMAL collect | distribute #-}
  collect :: (Along f) => (x -> d y) -> f x -> d (f y)
  collect f = distribute . morphism f
  distribute :: (Along f) => f (d x) -> d (f x)
  distribute = collect id

cotraverse ::
  (Collectable d, Along f) =>
  (f x -> y) -> f (d x) -> d y
cotraverse f = morphism f . distribute

instance Collectable ((->) z) where
  collect :: (Along f) => (x -> z -> y) -> (f x -> z -> f y)
  collect f fx z = morphism (`f` z) fx
  distribute :: (Along f) => f (z -> x) -> z -> f x
  distribute fz_x z = morphism ($ z) fz_x
instance Collectable Proxy where
  collect :: (Along f) => (x -> Proxy y) -> f x -> Proxy (f y)
  collect _ _ = Proxy
  distribute :: (Along f) => f (Proxy x) -> Proxy (f x)
  distribute _ = Proxy
instance Collectable Identity where
  collect :: (Along f) => (x -> Identity y) -> f x -> Identity (f y)
  collect f = Identity . morphism (runIdentity . f)
  distribute :: (Along f) => f (Identity x) -> Identity (f x)
  distribute = Identity . morphism runIdentity
instance Collectable Complex where
  distribute :: (Along f) => f (Complex x) -> Complex (f x)
  distribute fc = morphism realPart fc :+ morphism imagPart fc
instance
  (Data.Functor f, Data.Functor g, Collectable f, Collectable g) =>
  Collectable (Product f g)
  where
  distribute ::
    (Along h) => h (Product f g x) -> Product f g (h x)
  distribute hp = Pair (collect fstP hp) (collect sndP hp)
   where
    fstP (Pair x _) = x
    sndP (Pair _ y) = y
instance
  (Data.Functor f, Data.Functor g, Collectable f, Collectable g) =>
  Collectable (Compose f g)
  where
  distribute :: (Along h) => h (Compose f g x) -> Compose f g (h x)
  distribute = Compose . morphism distribute . collect getCompose
  collect ::
    (Along h) => (x -> Compose f g y) -> h x -> Compose f g (h y)
  collect f =
    Compose . morphism distribute . collect (getCompose . f)

instance Collectable U1 where
  collect :: (Along f) => (x -> U1 y) -> f x -> U1 (f y)
  collect _ _ = U1
instance Collectable Par1 where
  collect :: (Along f) => (x -> Par1 y) -> f x -> Par1 (f y)
  collect x_Py = Par1 #. morphism (unPar1 #. x_Py)
instance (Collectable f) => Collectable (Rec1 f) where
  collect :: (Along g) => (x -> Rec1 f y) -> g x -> Rec1 f (g y)
  collect x_Rfy gx = Rec1 (collect (unRec1 . x_Rfy) gx)
instance (Collectable f) => Collectable (M1 i c f) where
  collect :: (Along g) => (x -> M1 i c f y) -> g x -> M1 i c f (g y)
  collect x_Micgy gx = M1 (collect (unM1 . x_Micgy) gx)
instance (Collectable f, Collectable g) => Collectable (f :*: g) where
  distribute :: (Along h) => h ((f :*: g) x) -> (f :*: g) (h x)
  distribute hp = collect fstP hp :*: collect sndP hp
   where
    fstP (x :*: _) = x
    sndP (_ :*: y) = y
instance (Collectable f, Collectable g) => Collectable (f :.: g) where
  distribute :: (Along h) => h ((f :.: g) x) -> (f :.: g) (h x)
  distribute = Comp1 . morphism distribute . collect unComp1
  collect :: (Along h) => (x -> (f :.: g) y) -> h x -> (f :.: g) (h y)
  collect x_fgy = Comp1 . morphism distribute . collect (coerce x_fgy)

type Tabulation :: (Type -> Type) -> Constraint
class (Collectable f) => Tabulation f where
  type Table f :: Type
  fromTable :: (Table f -> x) -> f x
  toTable :: f x -> (Table f -> x)

instance Tabulation ((->) z) where
  type Table ((->) z) = z
  fromTable :: (Table ((->) z) -> x) -> (z -> x)
  fromTable = id
  toTable :: (z -> x) -> (Table ((->) z) -> x)
  toTable = id
instance Tabulation Identity where
  type Table Identity = ()
  fromTable :: (Table Identity -> x) -> Identity x
  fromTable f = Identity (f ())
  toTable :: Identity x -> Table Identity -> x
  toTable (Identity x) () = x
instance Tabulation Proxy where
  type Table Proxy = Void
  fromTable :: (Table Proxy -> x) -> Proxy x
  fromTable _ = Proxy
  toTable :: Proxy x -> Table Proxy -> x
  toTable Proxy = absurd
instance Tabulation Complex where
  type Table Complex = Either () ()
  fromTable :: (Table Complex -> x) -> Complex x
  fromTable f = f (Left ()) :+ f (Right ())
  toTable :: Complex x -> Table Complex -> x
  toTable (r :+ i) = \case
    Left () -> r
    Right () -> i
instance
  (Data.Functor f, Data.Functor g, Tabulation f, Tabulation g) =>
  Tabulation (Product f g)
  where
  type Table (Product f g) = Either (Table f) (Table g)
  fromTable :: (Table (Product f g) -> x) -> Product f g x
  fromTable f = Pair (fromTable (f . Left)) (fromTable (f . Right))
  toTable :: Product f g x -> Table (Product f g) -> x
  toTable (Pair fx gx) = \case
    Left f -> toTable fx f
    Right g -> toTable gx g
instance
  (Data.Functor f, Data.Functor g, Tabulation f, Tabulation g) =>
  Tabulation (Compose f g)
  where
  type Table (Compose f g) = (Table f, Table g)
  fromTable :: (Table (Compose f g) -> x) -> Compose f g x
  fromTable = Compose . fromTable . morphism fromTable . curry
  toTable :: Compose f g x -> Table (Compose f g) -> x
  toTable (Compose fgx) (tf, tg) = toTable (toTable fgx tf) tg

class
  (Along f, Tabulation t) =>
  Adjunction f t
    | f -> t
    , t -> f
  where
  {-# MINIMAL (unit, counit) | (left, right) #-}
  unit :: x -> t (f x)
  unit = left id
  counit :: f (t x) -> x
  counit = right id
  left :: (f x -> y) -> x -> t y
  left f = morphism f . unit
  right :: (x -> t y) -> f x -> y
  right f = counit . morphism f
instance Adjunction Identity Identity where
  unit :: x -> Identity (Identity x)
  unit = Identity . Identity
  counit :: Identity (Identity x) -> x
  counit = runIdentity . runIdentity
instance Adjunction ((,) z) ((->) z) where
  unit :: x -> z -> (z, x)
  unit = flip (,)
  counit :: (z, z -> x) -> x
  counit = uncurry (flip ($))

zipR :: (Adjunction f t) => (t x, t y) -> t (x, y)
zipR = left (right fst &&& right snd)

unzipR :: (Along f) => f (x, y) -> (f x, f y)
unzipR = morphism fst &&& morphism snd

cozipL :: (Adjunction f t) => f (Either x y) -> Either (f x) (f y)
cozipL = right (left Left ||| left Right)

uncozipL :: (Along f) => Either (f x) (f y) -> f (Either x y)
uncozipL = morphism Left ||| morphism Right

class (Against f) => Cotabulation f where
  type Cotable f :: Type
  fromCotable :: (x -> Cotable f) -> f x
  toCotable :: f x -> x -> Cotable f
instance Cotabulation (Op z) where
  type Cotable (Op z) = z
  fromCotable :: (x -> Cotable (Op z)) -> Op z x
  fromCotable = Op
  toCotable :: Op z x -> x -> Cotable (Op z)
  toCotable = getOp
instance Cotabulation Proxy where
  type Cotable Proxy = ()
  fromCotable :: (x -> Cotable Proxy) -> Proxy x
  fromCotable = const Proxy
  toCotable :: Proxy x -> x -> Cotable Proxy
  toCotable Proxy = const ()

class (Against f, Cotabulation g) => Coadjunction f g where
  unitCo :: x -> g (f x)
  unitCo = leftCo id
  counitCo :: x -> f (g x)
  counitCo = rightCo id
  leftCo :: (y -> f x) -> x -> g y
  leftCo f = morphism (Op f) . unitCo
  rightCo :: (x -> g y) -> y -> f x
  rightCo f = morphism (Op f) . counitCo
instance Coadjunction (Op z) (Op z) where
  unitCo :: x -> Op z (Op z x)
  unitCo x = Op \(Op x_z) -> x_z x
  counitCo :: x -> Op z (Op z x)
  counitCo = unitCo

-- Profunctors

phormism ::
  forall x a p.
  (Morphisms Op (-->) p) =>
  (a -> x) -> (forall y. p x y -> p a y)
phormism a_x = transform (morphism (Op a_x) :: p x --> p a)

type Fletched :: (Type -> Type -> Type) -> Constraint
type Fletched p = (forall z. Along (p z), Morphisms Op (-->) p)

(#.) ::
  (Fletched p, Coercible y y') =>
  q y y' -> p x y -> p x y'
(#.) _ !p = morphism coerce p
(.#) ::
  (Fletched p, Coercible x x') =>
  p x' y -> q x x' -> p x y
(.#) !p _ = phormism coerce p

fletch :: (Fletched p) => (a -> x) -> (y -> b) -> p x y -> p a b
fletch a_x y_b = phormism a_x . morphism y_b

instance Morphisms Op (-->) (->) where
  morphism :: Op x y -> (->) x --> (->) y
  morphism (Op y_x) = Transform (. y_x)
instance (Along f) => Morphisms Op (-->) (Kleisli f) where
  morphism :: Op x y -> Kleisli f x --> Kleisli f y
  morphism (Op y_x) =
    Transform \(Kleisli x_fz) -> Kleisli (x_fz . y_x)
instance (Along f) => Morphisms Op (-->) (Cokleisli f) where
  morphism :: Op x y -> Cokleisli f x --> Cokleisli f y
  morphism (Op y_x) =
    Transform \(Cokleisli fx_z) -> Cokleisli (fx_z . morphism y_x)
instance Morphisms Op (-->) (Ix i) where
  morphism :: Op x y -> Ix i x --> Ix i y
  morphism (Op y_x) =
    Transform \(Ix i_x_z) -> Ix \i -> i_x_z i . y_x

type Forget :: Type -> Type -> k -> Type
newtype Forget z x y = Forget {runForget :: x -> z}

instance Data.Functor (Forget z x) where
  fmap :: (y -> y') -> Forget z x y -> Forget z x y'
  fmap _ (Forget x_z) = Forget x_z
instance (Semigroup z) => Semigroup (Forget z x y) where
  (<>) :: Forget z x y -> Forget z x y -> Forget z x y
  Forget z <> Forget z' = Forget (z <> z')
instance (Monoid z) => Monoid (Forget z x y) where
  mempty :: Forget z x y
  mempty = Forget mempty
instance Morphisms (->) (->) (Forget z x) where
  morphism :: (y -> y') -> Forget z x y -> Forget z x y'
  morphism _ (Forget x_z) = Forget x_z
instance Morphisms Op (->) (Forget z x) where
  morphism :: Op y y' -> Forget z x y -> Forget z x y'
  morphism _ (Forget x_z) = Forget x_z
instance Morphisms Op (-->) (Forget z) where
  morphism :: Op x y -> Forget z x --> Forget z y
  morphism (Op y_x) =
    Transform \(Forget x_z) -> Forget (x_z . y_x)

type Strong :: (Type -> Type -> Type) -> Constraint
class (Fletched p) => Strong p where
  product0 :: p x y -> p (x, z) (y, z)
  product1 :: p x y -> p (z, x) (z, y)

instance Strong (->) where
  product0 :: (x -> y) -> (x, z) -> (y, z)
  product0 x_y = morphism' x_y
  product1 :: (x -> y) -> (z, x) -> (z, y)
  product1 = morphism
instance (Monad f) => Strong (Kleisli f) where
  product0 :: Kleisli f x y -> Kleisli f (x, z) (y, z)
  product0 (Kleisli x_fy) =
    Kleisli \(x, z) -> morphism (,z) (x_fy x)
  product1 :: Kleisli f x y -> Kleisli f (z, x) (z, y)
  product1 (Kleisli x_fy) =
    Kleisli \(z, x) -> morphism (z,) (x_fy x)
instance Strong (Forget z) where
  product0 :: Forget z x y -> Forget z (x, z') (y, z')
  product0 (Forget z) = Forget (z . fst)
  product1 :: Forget z x y -> Forget z (z', x) (z', y)
  product1 (Forget z) = Forget (z . snd)
instance Strong (Ix i) where
  product0 :: Ix i x y -> Ix i (x, z) (y, z)
  product0 (Ix i_x_y) = Ix \i (x, z) -> (i_x_y i x, z)
  product1 :: Ix i x y -> Ix i (z, x) (z, y)
  product1 (Ix i_x_y) = Ix \i (z, x) -> (z, i_x_y i x)

type Costrong :: (Type -> Type -> Type) -> Constraint
class (Fletched p) => Costrong p where
  unproduct0 :: p (x, z) (y, z) -> p x y
  unproduct1 :: p (z, x) (z, y) -> p x y

instance Costrong (->) where
  unproduct0 :: ((x, z) -> (y, z)) -> x -> y
  unproduct0 x_y x = let (y, __) = x_y (x, __) in y
  unproduct1 :: ((z, x) -> (z, y)) -> x -> y
  unproduct1 x_y x = let (__, y) = x_y (__, x) in y
instance (Along f, MonadFix f) => Costrong (Kleisli f) where
  unproduct0 :: Kleisli f (x, z) (y, z) -> Kleisli f x y
  unproduct0 (Kleisli x_fy) =
    Kleisli (Data.fmap fst . mfix . \x y -> x_fy (x, snd y))
  unproduct1 :: Kleisli f (z, x) (z, y) -> Kleisli f x y
  unproduct1 (Kleisli x_fy) =
    Kleisli (Data.fmap snd . mfix . \x y -> x_fy (fst y, x))
instance (Along f) => Costrong (Cokleisli f) where
  unproduct0 :: Cokleisli f (x, z) (y, z) -> Cokleisli f x y
  unproduct0 (Cokleisli fx_y) =
    Cokleisli \fx -> let (y, __) = fx_y (morphism (,__) fx) in y
  unproduct1 :: Cokleisli f (z, x) (z, y) -> Cokleisli f x y
  unproduct1 (Cokleisli fx_y) =
    Cokleisli \fx -> let (__, y) = fx_y (morphism (__,) fx) in y
instance Costrong (Ix i) where
  unproduct0 :: Ix i (x, z) (y, z) -> Ix i x y
  unproduct0 (Ix i_xz_yz) =
    Ix \i x -> let (y, z) = i_xz_yz i (x, z) in y
  unproduct1 :: Ix i (z, x) (z, y) -> Ix i x y
  unproduct1 (Ix i_zx_zy) =
    Ix \i x -> let (z, y) = i_zx_zy i (z, x) in y

type Choice :: (Type -> Type -> Type) -> Constraint
class (Fletched p) => Choice p where
  inl :: p x y -> p (Either x z) (Either y z)
  inr :: p x y -> p (Either z x) (Either z y)

instance Choice (->) where
  inl :: (x -> y) -> Either x z -> Either y z
  inl x_y = morphism' x_y
  inr :: (x -> y) -> Either z x -> Either z y
  inr = morphism
instance (Monad f) => Choice (Kleisli f) where
  inl :: Kleisli f x y -> Kleisli f (Either x z) (Either y z)
  inl (Kleisli x_fy) = Kleisli \case
    Left x -> morphism Left (x_fy x)
    Right r -> pure (Right r)
  inr :: Kleisli f x y -> Kleisli f (Either z x) (Either z y)
  inr (Kleisli x_fy) = Kleisli \case
    Left l -> pure (Left l)
    Right x -> morphism Right (x_fy x)
instance Choice (Ix i) where
  inl :: Ix i x y -> Ix i (Either x z) (Either y z)
  inl (Ix i_x_y) = Ix \i -> \case
    Left x -> Left (i_x_y i x)
    Right r -> Right r
  inr :: Ix i x y -> Ix i (Either z x) (Either z y)
  inr (Ix i_x_y) = Ix \i -> \case
    Left l -> Left l
    Right x -> Right (i_x_y i x)

type Cochoice :: (Type -> Type -> Type) -> Constraint
class (Fletched p) => Cochoice p where
  outl :: p (Either x z) (Either y z) -> p x y
  outr :: p (Either z x) (Either z y) -> p x y

instance Cochoice (->) where
  outl :: (Either x z -> Either y z) -> x -> y
  outl f = rec . Left where rec = either id (rec . Right) . f
  outr :: (Either z x -> Either z y) -> x -> y
  outr f = rec . Right where rec = either (rec . Left) id . f
instance (Applicative f) => Cochoice (Cokleisli f) where
  outl :: Cokleisli f (Either x z) (Either y z) -> Cokleisli f x y
  outl (Cokleisli f) = Cokleisli (rec . morphism Left)
   where
    rec = either id (rec . pure . Right) . f
  outr :: Cokleisli f (Either z x) (Either z y) -> Cokleisli f x y
  outr (Cokleisli f) = Cokleisli (rec . morphism Right)
   where
    rec = either (rec . pure . Left) id . f
instance Cochoice (Forget z) where
  outl :: Forget z (Either x z') (Either y z') -> Forget z x y
  outl (Forget z) = Forget (z . Left)
  outr :: Forget z (Either z' x) (Either z' y) -> Forget z x y
  outr (Forget z) = Forget (z . Right)
instance Cochoice (Ix i) where
  outl :: Ix i (Either x z) (Either y z) -> Ix i x y
  outl (Ix i_exzeyz) = Ix \i -> rec i . Left
   where
    rec i = either id (rec i . Right) . i_exzeyz i
  outr :: Ix i (Either z x) (Either z y) -> Ix i x y
  outr (Ix i_ezxezy) = Ix \i -> rec i . Right
   where
    rec i = either (rec i . Left) id . i_ezxezy i

type Sieve ::
  (Type -> Type -> Type) -> (Type -> Type) -> Constraint
class (Fletched p, Along f) => Sieve p f | p -> f where
  sieve :: p x y -> x -> f y

instance Sieve (->) Identity where
  sieve :: (x -> y) -> x -> Identity y
  sieve = (Identity .)
instance (Monad f) => Sieve (Kleisli f) f where
  sieve :: Kleisli f x y -> x -> f y
  sieve = runKleisli
instance Sieve (Forget z) (Const z) where
  sieve :: Forget z x y -> x -> Const z y
  sieve (Forget x_z) x = Const (x_z x)
instance Sieve (Ix i) ((->) i) where
  sieve :: Ix i x y -> x -> i -> y
  sieve (Ix i_x_y) = flip i_x_y

type Cosieve ::
  (Type -> Type -> Type) -> (Type -> Type) -> Constraint
class (Fletched p, Along f) => Cosieve p f | p -> f where
  cosieve :: p x y -> f x -> y

instance Cosieve (->) Identity where
  cosieve :: (x -> y) -> Identity x -> y
  cosieve = (. runIdentity)
instance (Along f) => Cosieve (Cokleisli f) f where
  cosieve :: Cokleisli f x y -> f x -> y
  cosieve = runCokleisli
instance Cosieve (Ix i) ((,) i) where
  cosieve :: Ix i x y -> (i, x) -> y
  cosieve (Ix i_x_y) = uncurry i_x_y

type Representable :: (Type -> Type -> Type) -> Constraint
class
  (Sieve p (Representation p), Strong p) =>
  Representable p
  where
  type Representation p :: Type -> Type
  represent :: (x -> Representation p y) -> p x y

instance Representable (->) where
  type Representation (->) = Identity
  represent :: (x -> Representation (->) y) -> x -> y
  represent = (runIdentity .)
instance (Monad f) => Representable (Kleisli f) where
  type Representation (Kleisli f) = f
  represent :: (x -> Representation (Kleisli f) y) -> Kleisli f x y
  represent = Kleisli
instance Representable (Forget z) where
  type Representation (Forget z) = Const z
  represent :: (x -> Representation (Forget z) y) -> Forget z x y
  represent = Forget . (getConst .)
instance Representable (Ix i) where
  type Representation (Ix i) = (->) i
  represent :: (x -> i -> y) -> Ix i x y
  represent = Ix . flip

class
  (Cosieve p (Corepresentation p), Costrong p) =>
  Corepresentable p
  where
  type Corepresentation p :: Type -> Type
  corepresent :: (Corepresentation p x -> y) -> p x y

instance Corepresentable (->) where
  type Corepresentation (->) = Identity
  corepresent :: (Corepresentation (->) x -> y) -> x -> y
  corepresent = (. Identity)
instance (Along f) => Corepresentable (Cokleisli f) where
  type Corepresentation (Cokleisli f) = f
  corepresent ::
    (Corepresentation (Cokleisli f) x -> y) -> Cokleisli f x y
  corepresent = Cokleisli
instance Corepresentable (Ix i) where
  type Corepresentation (Ix i) = (,) i
  corepresent :: ((i, x) -> y) -> Ix i x y
  corepresent = Ix . curry

type Closed :: (Type -> Type -> Type) -> Constraint
class (Fletched p) => Closed p where
  closed :: p x y -> p (z -> x) (z -> y)

instance Closed (->) where
  closed :: (x -> y) -> (z -> x) -> (z -> y)
  closed = (.)
instance (Collectable f, Monad f) => Closed (Kleisli f) where
  closed :: Kleisli f x y -> Kleisli f (z -> x) (z -> y)
  closed (Kleisli x_fy) = Kleisli (distribute . (x_fy .))
instance (Along f) => Closed (Cokleisli f) where
  closed :: Cokleisli f x y -> Cokleisli f (z -> x) (z -> y)
  closed (Cokleisli fx_y) = Cokleisli \fzx z -> fx_y (morphism ($ z) fzx)
instance Closed (Ix i) where
  closed :: Ix i x y -> Ix i (z -> x) (z -> y)
  closed (Ix i_x_y) = Ix \i z_x -> i_x_y i . z_x

type Conjoined :: (Type -> Type -> Type) -> Constraint
class
  ( Fletched p
  , Strong p
  , Costrong p
  , Choice p
  , Closed p
  , Representable p
  , Monad (Representation p)
  , MonadFix (Representation p)
  , Collectable (Representation p)
  , Corepresentable p
  , Comonad (Corepresentation p)
  , Traversable (Corepresentation p)
  , Category p
  ) =>
  Conjoined p
  where
  promap :: forall x y f. (Along f) => p x y -> p (f x) (f y)
  promap = represent . collect . sieve
  conjoined :: q (x -> y) z -> q (p x y) z -> q (p x y) z
  conjoined _ q = q

instance Conjoined (->) where
  promap :: (Along f) => (x -> y) -> f x -> f y
  promap = morphism
  conjoined :: q (x -> y) z -> q (x -> y) z -> q (x -> y) z
  conjoined x _ = x
instance Conjoined (Ix i) where
  promap :: (Along f) => Ix i x y -> Ix i (f x) (f y)
  promap (Ix i_x_y) = Ix (morphism . i_x_y)

type Ixed :: Type -> (Type -> Type -> Type) -> Constraint
class (Conjoined p) => Ixed i p where
  ixed :: p x y -> i -> x -> y

instance Ixed i (->) where
  ixed :: (x -> y) -> i -> x -> y
  ixed = const
instance (i ~ j) => Ixed j (Ix i) where
  ixed :: Ix i x y -> i -> x -> y
  ixed = ix

infixr 9 <.
(<.) ::
  (Ixed i p) =>
  (Ix i xs ys -> z) -> ((x -> y) -> xs -> ys) -> p x y -> z
(<.) iixsys x_y_xs_ys p = iixsys (Ix (x_y_xs_ys . ixed p))

infixr 9 .>
(.>) ::
  (Category p, Objects p x, Objects p y, Objects p z) =>
  p y z -> p x y -> p x z
(.>) = (.)

withIndex ::
  (Ixed i p, Along f) =>
  p (i, xs) (f (j, ys)) -> Ix i xs (f ys)
withIndex p = Ix \i xs -> morphism snd (ixed p i (i, xs))

selfIndex :: (Ixed x p) => p x y -> x -> y
selfIndex p = Control.join (ixed p)

asIndex ::
  (Ixed i p, Phantom f) => p i (f i) -> Ix i xs (f xs)
asIndex p = Ix \i _ -> phantom (ixed p i i)

reindexed ::
  (Ixed j p) =>
  (i -> j) -> (Ix i x y -> z) -> p x y -> z
reindexed i_j iixy_z p = iixy_z (Ix (ixed p . i_j))

icompose ::
  (Ixed k p) =>
  (i -> j -> k) ->
  (Ix i xs ys -> z) ->
  (Ix j x y -> xs -> ys) ->
  p x y ->
  z
icompose i_j_k iixy_z ijxsys_x_y p =
  iixy_z (Ix \i -> ijxsys_x_y (Ix (ixed p . i_j_k i)))

infixr 9 <.>
(<.>) ::
  (Ixed (i, j) p) =>
  (Ix i xs ys -> z) ->
  (Ix j x y -> xs -> ys) ->
  p x y ->
  z
(<.>) = icompose (,)

data Procompose p q x y = forall z. Procompose (p z y) (q x z)

instance
  (Fletched p) =>
  Morphisms (->) (->) (Procompose p q x)
  where
  morphism :: (w -> y) -> Procompose p q x w -> Procompose p q x y
  morphism w_y (Procompose pzw qxz) = Procompose (morphism w_y pzw) qxz
instance
  (Fletched p, Fletched q) =>
  Morphisms Op (-->) (Procompose p q)
  where
  morphism :: Op x y -> Procompose p q x --> Procompose p q y
  morphism (Op y_x) =
    Transform \(Procompose pzx qxz) -> Procompose pzx (phormism y_x qxz)
instance (Strong p, Strong q) => Strong (Procompose p q) where
  product0 :: Procompose p q x y -> Procompose p q (x, w) (y, w)
  product0 (Procompose pzy qxz) =
    Procompose (product0 pzy) (product0 qxz)
  product1 :: Procompose p q x y -> Procompose p q (w, x) (w, y)
  product1 (Procompose pzy qxz) =
    Procompose (product1 pzy) (product1 qxz)
instance
  (Corepresentable p, Corepresentable q) =>
  Costrong (Procompose p q)
  where
  unproduct0 :: Procompose p q (x, z) (y, z) -> Procompose p q x y
  unproduct0 pq = corepresent f
   where
    f fx = b where (b, d) = cosieve pq (morphism (,d) fx)
  unproduct1 :: Procompose p q (z, x) (z, y) -> Procompose p q x y
  unproduct1 pq = corepresent f
   where
    f fx = b where (d, b) = cosieve pq (morphism (d,) fx)
instance (Choice p, Choice q) => Choice (Procompose p q) where
  inl :: Procompose p q x y -> Procompose p q (Either x z) (Either y z)
  inl (Procompose pzy qxz) = Procompose (inl pzy) (inl qxz)
  inr :: Procompose p q x y -> Procompose p q (Either z x) (Either z y)
  inr (Procompose pzy qxz) = Procompose (inr pzy) (inr qxz)
instance
  (Sieve p f, Sieve q g) =>
  Sieve (Procompose p q) (Compose g f)
  where
  sieve :: Procompose p q x y -> x -> Compose g f y
  sieve (Procompose pzy qxz) x = Compose (morphism (sieve pzy) (sieve qxz x))
instance
  (Cosieve p f, Cosieve q g) =>
  Cosieve (Procompose p q) (Compose f g)
  where
  cosieve :: Procompose p q x y -> Compose f g x -> y
  cosieve (Procompose pzy qxz) (Compose fgx) =
    cosieve pzy (morphism (cosieve qxz) fgx)
instance
  (Representable p, Representable q) =>
  Representable (Procompose p q)
  where
  type
    Representation (Procompose p q) =
      Compose (Representation q) (Representation p)
  represent ::
    (x -> Representation (Procompose p q) y) ->
    Procompose p q x y
  represent x_Cpqy =
    Procompose (represent id) (represent (getCompose . x_Cpqy))
instance
  (Corepresentable p, Corepresentable q) =>
  Corepresentable (Procompose p q)
  where
  type
    Corepresentation (Procompose p q) =
      Compose (Corepresentation p) (Corepresentation q)
  corepresent :: (Corepresentation (Procompose p q) x -> y) -> Procompose p q x y
  corepresent cpqx_y = Procompose (corepresent (cpqx_y . Compose)) (corepresent id)
instance (Closed p, Closed q) => Closed (Procompose p q) where
  closed :: Procompose p q x y -> Procompose p q (z -> x) (z -> y)
  closed (Procompose pzy qxz) = Procompose (closed pzy) (closed qxz)
