{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Redundant lambda" -}

module Flex.Math.Optics where

import Flex.Math.Category
import Flex.Math.Numbers
import Flex.Math.Optics.TH

import Control.Applicative qualified as Control
import Data.Bool (Bool (..), otherwise)
import Data.Coerce (coerce)
import Data.Either
import Data.Enum (Enum (..))
import Data.Eq (Eq (..))
import Data.Function (const, fix, flip, ($))
import Data.Functor qualified as Data
import Data.Functor.Const (Const (..))
import Data.Functor.Contravariant (Op (..))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Graph qualified as Data
import Data.Int (Int)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Kind (Type)
import Data.List (List)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List1 (List1)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid (Monoid (mempty))
import Data.Ord (Ord, (<))
import Data.Semigroup
  ( All (..)
  , Any (..)
  , Dual (..)
  , Endo (..)
  , First (..)
  , Semigroup ((<>))
  )
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable qualified as Data
import Data.Tuple (snd, uncurry)
import Data.Type.Equality (type (~))
import Data.Vector qualified as Vector
import GHC.Arr qualified as Array
import GHC.Base (($!))
import GHC.Err (error)
import Numeric.Natural (Natural)

-- | @
-- type Optical p q f xs ys x y = p x (f y) -> q xs (f ys)
-- @
type Optical ::
  (k0 -> k1 -> Type) ->
  (k0 -> k1 -> Type) ->
  (k2 -> k1) ->
  k0 ->
  k2 ->
  k0 ->
  k2 ->
  Type
type Optical p q f xs ys x y = p x (f y) -> q xs (f ys)

-- | @
-- type Optic p f xs ys x y = p x (f y) -> p xs (f ys)
-- @
type Optic p f xs ys x y = Optical p p f xs ys x y

type Optic' p f xs x = Optic p f xs xs x x

-- | @
-- type Iso xs ys x y =
--   forall p f. (Fletched p, Along f) => p x (f y) -> p xs (f ys)
--  @
type Iso xs ys x y =
  forall p f. (Fletched p, Along f) => Optic p f xs ys x y

type Iso' xs x = Iso xs xs x x

iso :: (xs -> x) -> (y -> ys) -> Iso xs ys x y
iso xs_x y_ys = fletch xs_x (morphism y_ys)

idl ::
  (Fletched q) =>
  Iso
    (Procompose (->) q x y)
    (Procompose (->) q' x' y')
    (q x y)
    (q' x' y')
idl =
  fletch
    (\(Procompose pzy qxz) -> morphism pzy qxz)
    (morphism (Procompose id))

idr ::
  (Fletched q) =>
  Iso
    (Procompose q (->) x y)
    (Procompose q' (->) x' y')
    (q x y)
    (q' x' y')
idr =
  fletch
    (\(Procompose pzy qxz) -> phormism qxz pzy)
    (morphism (`Procompose` id))

assoc ::
  Iso
    (Procompose p (Procompose q r) x y)
    (Procompose p' (Procompose q' r') x y)
    (Procompose (Procompose p q) r x y)
    (Procompose (Procompose p' q') r' x y)
assoc =
  fletch
    (\(Procompose p (Procompose q r)) -> Procompose (Procompose p q) r)
    (morphism \(Procompose (Procompose p q) r) -> Procompose p (Procompose q r))

-- | @
-- type Over p f xs ys x y = p x (f y) -> xs -> f ys
-- @
type Over p f xs ys x y = Optical p (->) f xs ys x y

type Over' p f xs x = Over p f xs xs x x

-- | @
-- type Focus f xs ys x y = (x -> f y) -> (xs -> f ys)
-- @
type Focus f xs ys x y = Over (->) f xs ys x y

type Focus' f xs x = Focus f xs xs x x

(%%~) :: Focus f xs ys x y -> (x -> f y) -> xs -> f ys
(%%~) = id

-- | @
-- type IxFocus i f xs ys x y =
--   forall p. (Ixed i p) => p x (f y) -> xs -> f ys
-- @
type IxFocus i f xs ys x y =
  forall p. (Ixed i p) => Optical p (->) f xs ys x y

type IxFocus' i f xs x = IxFocus i f xs xs x x

-- | @
-- type Lens xs ys x y =
--   forall f. (Along f) => (x -> f y) -> (xs -> f ys)
-- @
type Lens xs ys x y = forall f. (Along f) => Focus f xs ys x y

type Lens' xs x = Lens xs xs x x

united :: Lens' xs ()
united f v = morphism (\() -> v) (f ())

-- | @
-- type IxLens i xs ys x y =
--   forall p f. (Ixed i p, Along f) => p x (f y) -> (xs -> f ys)
-- @
type IxLens i xs ys x y =
  forall f. (Along f) => IxFocus i f xs ys x y

type IxLens' i xs x = IxLens i xs xs x x

-- | @
-- type IxPreservingLens xs ys x y =
--   forall p f. (Conjoined p, Along f) => p x (f y) -> p xs (f ys)
-- @
type IxPreservingLens xs ys x y =
  forall p f. (Conjoined p, Along f) => Optic p f xs ys x y

type IxPreservingLens' xs x = IxPreservingLens xs xs x x

lens :: (xs -> x) -> (xs -> y -> ys) -> Lens xs ys x y
lens xs_x xs_y_ys pxfy xs = morphism (xs_y_ys xs) (pxfy (xs_x xs))

ixlens :: (xs -> (i, x)) -> (xs -> y -> ys) -> IxLens i xs ys x y
ixlens xs_ix xs_y_ys pxfy xs =
  morphism (xs_y_ys xs) (uncurry (ixed pxfy) (xs_ix xs))

ixplens :: (xs -> x) -> (xs -> y -> ys) -> IxPreservingLens xs ys x y
ixplens xs_x xs_y_ys pxfy = corepresent \fxs ->
  morphism (xs_y_ys (copure fxs)) (cosieve pxfy (morphism xs_x fxs))

-- | @
-- type Prism xs ys x y =
--   forall p f. (Choice p, Applicative f) => p x (f y) -> p xs (f ys)
-- @
type Prism xs ys x y =
  forall p f. (Choice p, Applicative f) => Optic p f xs ys x y

type Prism' xs x = Prism xs xs x x

filtered :: (xs -> Bool) -> Prism' xs xs
filtered p = fletch (\xs -> if p xs then Right xs else Left xs) (either pure id) . inr

ifiltered ::
  (Ixed i p, Applicative f) => (i -> xs -> Bool) -> Optical p (Ix i) f xs xs xs xs
ifiltered p pxsfxs = Ix \i x -> if p i x then ixed pxsfxs i x else pure x

prism :: (y -> ys) -> (xs -> Either ys x) -> Prism xs ys x y
prism y_ys xs_eysx = fletch xs_eysx (either pure (morphism y_ys)) . inr

prism' :: (y -> xs) -> (xs -> Maybe x) -> Prism xs xs x y
prism' y_ys xs_mx = prism y_ys \xs -> maybe (Left xs) Right (xs_mx xs)

matchingF :: Focus (Either x) xs ys x y -> xs -> Either ys x
matchingF f = either Right Left . f Left

_Left :: Prism (Either x z) (Either y z) x y
_Left = prism Left (either Right (Left . Right))

_Right :: Prism (Either z x) (Either z y) x y
_Right = prism Right (either (Left . Left) Right)

_Just :: Prism (Maybe x) (Maybe y) x y
_Just = prism Just (maybe (Left Nothing) Right)

-- | @
-- type Traversal xs ys x y =
--   forall f. (Applicative f) => (x -> f y) -> xs -> f ys
-- @
type Traversal xs ys x y =
  forall f. (Applicative f) => Focus f xs ys x y

type Traversal' xs x = Traversal xs xs x x

-- | @
-- type IxTraversal i xs ys x y =
--   forall p f.
--   (Ixed i p, Applicative f) =>
--   p x (f y) -> xs -> f ys
-- @
type IxTraversal i xs ys x y =
  forall f. (Applicative f) => IxFocus i f xs ys x y

filteredBy :: Getting (Maybe (First i)) x i -> IxTraversal' i x x
filteredBy g f x = case x ^? g of
  Nothing -> pure x
  Just y -> ixed f y x

type IxTraversal' i xs x = IxTraversal i xs xs x x

itraverseOf ::
  Over (Ix i) f xs ys x y -> (i -> x -> f y) -> xs -> f ys
itraverseOf l = l .# Ix

iforOf ::
  Over (Ix i) f xs ys x y -> xs -> (i -> x -> f y) -> f ys
iforOf = flip . itraverseOf

imapAccumMOf ::
  Over (Ix i) (StateT s f) xs ys x y ->
  (i -> x -> s -> f (s, y)) ->
  xs ->
  s ->
  f (s, ys)
imapAccumMOf l i_x_s_fsy xs =
  runStateT (l (StateT #. Ix i_x_s_fsy) xs)

imapAccumOf ::
  Over (Ix i) (State s) xs ys x y ->
  (i -> x -> s -> (s, y)) ->
  xs ->
  s ->
  (s, ys)
imapAccumOf l i_x_s_sy xs =
  runIdentity
    . runStateT
      (l (StateT #. Ix (((.) Identity .) . i_x_s_sy)) xs)

-- | @
-- type IxPreservingTraversal xs ys x y =
--   forall p f.
--   (Conjoined p, Applicative f) =>
--   p x (f y) -> xs -> f ys
-- @
type IxPreservingTraversal xs ys x y =
  forall p f. (Conjoined p, Applicative f) => Over p f xs ys x y

type IxPreservingTraversal' xs x = IxPreservingTraversal xs xs x x

-- | @
-- type Traversal1 xs ys x y =
--   forall f. (Apply f) => (x -> f y) -> xs -> f ys
-- @
type Traversal1 xs ys x y = forall f. (Apply f) => Focus f xs ys x y

type Traversal1' xs x = Traversal1 xs xs x x

-- | @
-- type IxTraversal1 xs ys x y =
--   forall p f. (Ixed i p, Apply f) => p x (f y) -> xs -> f ys
-- @
type IxTraversal1 i xs ys x y =
  forall f. (Apply f) => IxFocus i f xs ys x y

type IxTraversal1' i xs x = IxTraversal1 i xs xs x x

-- | @
-- type IxPreservingTraversal1 xs ys x y =
--   forall p f.
--   (Conjoined p, Apply f) =>
--   p x (f y) -> xs -> f ys
-- @
type IxPreservingTraversal1 xs ys x y =
  forall p f. (Conjoined p, Apply f) => Over p f xs ys x y

type IxPreservingTraversal1' xs x = IxPreservingTraversal1 xs xs x x

onIndices ::
  (Ixed i p, Applicative f) =>
  (i -> Bool) -> Optical p (Ix i) f x x x x
onIndices p pxfx =
  Ix \i x -> if p i then ixed pxfx i x else pure x

onIndex ::
  (Ixed i p, Eq i, Applicative f) =>
  i -> Optical p (Ix i) f x x x x
onIndex i = onIndices (i ==)

traversal :: ((x -> f y) -> xs -> f ys) -> Focus f xs ys x y
traversal = id

traverseOf :: Focus f xs ys x y -> (x -> f y) -> xs -> f ys
traverseOf = id

forOf :: Focus f xs ys x y -> xs -> (x -> f y) -> f ys
forOf = flip

sequenceOf :: Focus f xs ys (f y) y -> xs -> f ys
sequenceOf f = f id

class Indices xs where
  type Index xs :: Type
  type Value xs :: Type
  index :: Index xs -> Traversal' xs (Value xs)
  default index ::
    (At xs) => Index xs -> Traversal xs xs (Value xs) (Value xs)
  index i = at i . traverse

iindex ::
  (Indices xs) =>
  Index xs -> IxTraversal (Index xs) xs xs (Value xs) (Value xs)
iindex i pvfv = index i (ixed pvfv i)

instance (Eq z) => Indices (z -> x) where
  type Index (z -> x) = z
  type Value (z -> x) = x
  index :: z -> Traversal' (z -> x) x
  index z x_fx z_x =
    morphism (\x z' -> if z == z' then x else z_x z') (x_fx (z_x z))
instance Indices (Identity x) where
  type Index (Identity x) = ()
  type Value (Identity x) = x
  index :: () -> Traversal' (Identity x) x
  index () f (Identity x) = morphism Identity (f x)
instance Indices (Maybe x) where
  type Index (Maybe x) = ()
  type Value (Maybe x) = x
  index :: () -> Traversal' (Maybe x) x
  index () f = \case
    Nothing -> pure Nothing
    Just x -> morphism Just (f x)
instance Indices (List x) where
  type Index (List x) = Natural
  type Value (List x) = x
  index :: Natural -> Traversal' [x] x
  index n x_fx xs0 = go xs0 n
   where
    go [] _ = pure []
    go (x : xs) 0 = morphism (: xs) (x_fx x)
    go (x : xs) !i = morphism (x :) (go xs (pred i))
instance Indices (List1 x) where
  type Index (List1 x) = Natural
  type Value (List1 x) = x
  index :: Natural -> Traversal' (List1 x) x
  index n x_fx xs0 = go xs0 n
   where
    go (x :| xs) 0 = morphism (:| xs) (x_fx x)
    go (x :| xs) !i = morphism (x :|) (index (pred i) x_fx xs)
instance (Ord k) => Indices (Map k v) where
  type Index (Map k v) = k
  type Value (Map k v) = v
  index :: k -> Traversal' (Map k v) v
  index k v_fv m = case m Map.!? k of
    Nothing -> pure m
    Just v -> morphism (flip (Map.insert k) m) (v_fv v)
instance Indices (IntMap x) where
  type Index (IntMap x) = Int
  type Value (IntMap x) = x
  index :: Int -> Traversal' (IntMap x) x
  index k v_fv m = case m IntMap.!? k of
    Nothing -> pure m
    Just v -> morphism (flip (IntMap.insert k) m) (v_fv v)
instance (Ord k) => Indices (Set k) where
  type Index (Set k) = k
  type Value (Set k) = ()
  index :: (Ord k) => k -> Traversal' (Set k) ()
  index k u_fu s
    | Set.member k s = morphism (const s) (u_fu ())
    | otherwise = pure s
instance Indices (Seq x) where
  type Index (Seq x) = Natural
  type Value (Seq x) = x
  index :: Natural -> Traversal' (Seq x) x
  index k x_fx s = case Seq.lookup (from k) s of
    Nothing -> pure s
    Just x -> morphism (flip (Seq.update (from k)) s) (x_fx x)
instance Indices Data.Graph where
  type Index Data.Graph = Int
  type Value Data.Graph = [Int]
  index :: Int -> Traversal' Data.Graph [Int]
  index i vs_fvs g
    | Array.inRange (Array.bounds g) i =
        flip morphism (vs_fvs (g Array.! i)) \es ->
          g Array.// [(i, es)]
    | otherwise = pure g

instance Indices (Vector.Vector x) where
  type Index (Vector.Vector x) = Natural
  type Value (Vector.Vector x) = x
  index :: Natural -> Traversal' (Vector.Vector x) x
  index i v_fv' vs
    | from i < Vector.length vs = case vs Vector.! from i of
        v -> morphism (\v' -> vs Vector.// [(from i, v')]) (v_fv' v)
    | otherwise = pure vs

class (Indices xs) => At xs where
  at :: Index xs -> Lens xs xs (Maybe (Value xs)) (Maybe (Value xs))

iat ::
  (At xs) =>
  Index xs ->
  IxLens (Index xs) xs xs (Maybe (Value xs)) (Maybe (Value xs))
iat i f = at i (ixed f i)

instance At (Maybe x) where
  at :: () -> Lens (Maybe x) (Maybe x) (Maybe x) (Maybe x)
  at () = id
instance (Ord k) => At (Map k v) where
  at :: (Ord k) => k -> Lens (Map k v) (Map k v) (Maybe v) (Maybe v)
  at k f m =
    let mv = m Map.!? k
     in flip morphism (f mv) \case
          Nothing -> maybe m (const (Map.delete k m)) mv
          Just v -> Map.insert k v m
instance At (IntMap v) where
  at :: Int -> Lens (IntMap v) (IntMap v) (Maybe v) (Maybe v)
  at k f m =
    let mv = m IntMap.!? k
     in flip morphism (f mv) \case
          Nothing -> maybe m (const (IntMap.delete k m)) mv
          Just v -> IntMap.insert k v m

-- | @
-- type Fold xs x =
--   forall f. (Phantom f, Applicative f) => (x -> f x) -> (xs -> f xs)
-- @
type Fold xs x =
  forall f. (Phantom f, Applicative f) => Focus f xs xs x x

folding :: (Foldable t) => (xs -> t x) -> Fold xs x
folding xs_tx x_fx = phantom . traverse_ x_fx . xs_tx

foldring ::
  (Phantom f, Applicative f) =>
  ((x -> f x -> f x) -> f x -> xs -> f x) -> Focus f xs xs x x
foldring xfxfx_fx_xs_fx x_fx =
  phantom
    . xfxfx_fx_xs_fx
      (liftA2 (const id) . x_fx)
      (phantom (pure ()))

ifolding ::
  (Ixed i p, Foldable t, Phantom f, Applicative f) =>
  (xs -> t (i, x)) -> Over p f xs xs x x
ifolding xs_tix pxfx =
  phantom
    . traverse_
      (phantom . uncurry (ixed pxfx))
    . xs_tix

ifoldring ::
  (Ixed i p, Phantom f, Applicative f) =>
  ((i -> x -> f x -> f x) -> f x -> xs -> f x) -> Over p f xs xs x x
ifoldring ixfxfx_fx_xs_fx pxfy =
  phantom
    . ixfxfx_fx_xs_fx
      (\i x fx -> liftA2 (const id) (ixed pxfy i x) fx)
      (phantom (pure ()))

repeated :: (Apply f) => Focus f x x x x
repeated x_fx x = liftA2 (const id) (x_fx x) (repeated x_fx x)

replicated :: Natural -> Fold x x
replicated n x_fx x =
  let fx = x_fx x
      rec = \case
        0 -> phantom (pure ())
        m -> liftA2 (const id) fx (rec (pred m))
   in rec n

cycled :: (Apply f) => Focus f xs ys x y -> Focus f xs ys x y
cycled f x_fy xs = liftA2 (const id) (f x_fy xs) (cycled f x_fy xs)

unfolded :: (x -> Maybe (y, x)) -> Fold x y
unfolded x_myx y_fy = fix \rec x -> case x_myx x of
  Just (y, x') -> liftA2 (const id) (y_fy y) (rec x')
  Nothing -> phantom (pure ())

iterated :: (Apply f) => (x -> x) -> Focus f x x x x
iterated x_x x_fx =
  fix \rec x -> liftA2 (const id) (x_fx x) (rec (x_x x))

-- | @
-- type IxFold i xs ys x y =
--   forall p f.
--   (Ixed i p, Phantom f, Applicative f) =>
--   p x (f y) -> (xs -> f ys)
-- @
type IxFold i xs x =
  forall f. (Phantom f, Applicative f) => IxFocus i f xs xs x x

folded :: forall i f x. (IxFoldable i f) => IxFold i (f x) x
folded = conjoined (foldring foldr) (ifoldring @i ifoldr)

-- | @
-- type IxPreservingFold xs x =
--   forall f.
--   (Conjoined p, Phantom f, Applicative f) =>
--   p x (f x) -> p xs (f xs)
-- @
type IxPreservingFold xs x =
  forall p f.
  (Conjoined p, Phantom f, Applicative f) =>
  Optic p f xs xs x x

-- | @
-- type Fold1 xs x =
--   forall f. (Phantom f, Apply f) => (x -> f x) -> (xs -> f xs)
-- @
type Fold1 xs x =
  forall f. (Phantom f, Apply f) => Focus f xs xs x x

-- | @
-- type IxFold1 i xs ys x y =
--   forall f.
--   (Ixed i p, Phantom f, Apply f) =>
--   p x (f y) -> (xs -> f ys)
-- @
type IxFold1 i xs x =
  forall f. (Phantom f, Apply f) => IxFocus i f xs xs x x

-- | @
-- type IxPreservingFold1 xs x =
--   forall f.
--   (Ixed i p, Phantom f, Apply f) =>
--   p x (f x) -> p xs (f xs)
-- @
type IxPreservingFold1 xs x =
  forall p f.
  (Conjoined p, Phantom f, Apply f) =>
  Optic p f xs xs x x

-- | @
-- type Getter xs ys x y =
--   forall f. (Phantom f) => (x -> f x) -> (xs -> f xs)
-- @
type Getter xs x = forall f. (Phantom f) => Focus f xs xs x x

-- | @
-- type IxGetter i xs x =
--   forall p f. (Ixed i p, Phantom f) => p x (f x) -> (xs -> f xs)
-- @
type IxGetter i xs x =
  forall f. (Phantom f) => IxFocus i f xs x x x

-- | @
-- type IxPreservingGetter xs x =
--   forall p f. (Conjoined p, Phantom f) => p x (f x) -> p xs (f xs)
-- @
type IxPreservingGetter xs x =
  forall p f. (Conjoined p, Phantom f) => Optic p f xs xs x x

-- | @
-- type Getting z xs x = (x -> Const z x) -> (xs -> Const z xs)
-- @
type Getting z xs x = Focus (Const z) xs xs x x

-- | @
-- type IxGetting i z xs x =
--   Ix i x (Const z x) -> (xs -> Const z xs)
-- @
type IxGetting i z xs x = Over (Ix i) (Const z) xs xs x x

-- | @
-- type Accessing p z xs x =
--   p x (Const z x) -> (xs -> Const z xs)
-- @
type Accessing p z xs x = Over p (Const z) xs xs x x

getting ::
  (Fletched p, Fletched q, Phantom f) =>
  Optical p q f xs ys x y -> Optical p q f xs xs x x
getting o pxfx = morphism phantom (o (morphism phantom pxfx))

to ::
  forall p f xs x.
  (Fletched p, Against f) =>
  (xs -> x) -> Optic p f xs xs x x
to xs_x = fletch xs_x (morphism (Op xs_x))

ito ::
  forall i p f xs x.
  (Ixed i p, Against f) =>
  (xs -> (i, x)) -> Over p f xs xs x x
ito xs_ix =
  fletch xs_ix (morphism (Op (snd . xs_ix))) . uncurry . ixed

infixl 8 ^.
(^.) :: xs -> Getting x xs x -> x
(^.) = flip view

view :: Getting x xs x -> xs -> x
view g = getConst . g Const

views :: Focus (Const z) xs xs x x -> (x -> z) -> xs -> z
views f x_z = getConst . f (Const . x_z)

infixl 8 ^@.
(^@.) :: xs -> IxGetting i (i, x) xs x -> (i, x)
(^@.) = flip iview

iview :: IxGetting i (i, x) xs x -> xs -> (i, x)
iview ig = getConst . ig (Ix \i -> Const . (i,))

iviews :: IxGetting i z xs x -> (i -> x -> z) -> xs -> z
iviews ig i_xs_z = getConst . ig (Ix \i -> Const . i_xs_z i)

infixl 8 ^..
(^..) :: xs -> Getting (Endo [x]) xs x -> [x]
(^..) = flip toListOf

toListOf :: Getting (Endo [x]) xs x -> xs -> [x]
toListOf g = foldrOf g (:) []

foldlOf :: Getting (Dual (Endo z)) xs x -> (z -> x -> z) -> z -> xs -> z
foldlOf g z_x_z z = \xs ->
  let Dual (Endo z_z) = foldWithOf g (Dual #. Endo #. flip z_x_z) xs
   in z_z z

foldlOf' :: Getting (Endo (Endo z)) xs x -> (z -> x -> z) -> z -> xs -> z
foldlOf' l z_x_z z0 = \xs ->
  foldrOf l f (Endo id) xs `appEndo` z0
 where
  f x (Endo z_z) = Endo \z -> z_z $! z_x_z z x

foldrOf :: Getting (Endo z) xs x -> (x -> z -> z) -> z -> xs -> z
foldrOf g x_z_z z = \xs ->
  foldWithOf g (Endo #. x_z_z) xs `appEndo` z

foldWithOf :: Getting z xs x -> (x -> z) -> xs -> z
foldWithOf g x_z xs = getConst (g (Const . x_z) xs)

foldOf :: Getting x xs x -> xs -> x
foldOf g = getConst #. g Const

optionOf :: (Alternative f) => Getting (Option f x) xs x -> xs -> f x
optionOf g = getOption #. views g (Option #. pure)

andOf :: Getting All xs Bool -> xs -> Bool
andOf g = getAll #. foldWithOf g All

allOf :: Getting All xs x -> (x -> Bool) -> xs -> Bool
allOf g f = getAll #. foldWithOf g (All #. f)

orOf :: Getting Any xs Bool -> xs -> Bool
orOf g = getAny #. foldWithOf g Any

anyOf :: Getting Any xs x -> (x -> Bool) -> xs -> Bool
anyOf g f = getAny #. foldWithOf g (Any #. f)

traverseOf_ ::
  (Along f) =>
  Getting (f z) xs x -> (x -> f z) -> xs -> f ()
traverseOf_ g x_fz = morphism (const ()) . foldWithOf g x_fz

forOf_ ::
  (Along f) =>
  Getting (f z) xs x -> xs -> (x -> f z) -> f ()
forOf_ g = flip (traverseOf_ g)

sequenceOf_ ::
  (Along f) =>
  Getting (f x) xs (f x) -> xs -> f ()
sequenceOf_ g = morphism (const ()) . foldWithOf g id

osumOf :: (Alternative f) => Getting (Endo (f x)) xs (f x) -> xs -> f x
osumOf g = foldrOf g (<|>) nil

elemOf :: (Eq x) => Getting Any xs x -> x -> xs -> Bool
elemOf g = anyOf g . (==)

notElemOf :: (Eq x) => Getting All xs x -> x -> xs -> Bool
notElemOf g = allOf g . (/=)

has :: Getting Any xs x -> xs -> Bool
has g = getAny . foldWithOf g \_ -> Any True

hasn't :: Getting All xs x -> xs -> Bool
hasn't g = getAll . foldWithOf g \_ -> All False

lengthOf ::
  (Enum z, Additive z) =>
  Getting (Endo (Endo z)) xs x -> xs -> z
lengthOf g = foldlOf' g (const . succ) zero

findOf ::
  Getting (Endo (Maybe x)) xs x -> (x -> Bool) -> xs -> Maybe x
findOf g x_b =
  foldrOf g (\x fmx -> if x_b x then Just x else fmx) Nothing

findMOf ::
  (Monad f) =>
  Getting (Endo (f (Maybe x))) xs x ->
  (x -> f Bool) ->
  xs ->
  f (Maybe x)
findMOf g x_fb =
  foldrOf
    g
    (\x fmx -> x_fb x >>= \b -> if b then pure (Just x) else fmx)
    (pure Nothing)

infixl 8 ^?
(^?) :: xs -> Getting (Maybe (First x)) xs x -> Maybe x
xs ^? g = morphism getFirst (foldWithOf g (Just . First) xs)

infixl 8 ^?!
(^?!) :: xs -> Getting (Endo x) xs x -> x
xs ^?! g = foldrOf g const (error "(^?!): empty Fold") xs

pre ::
  Getting (Maybe (First x)) xs x ->
  IxPreservingGetter xs (Maybe x)
pre g =
  fletch
    ((morphism getFirst . getConst) #. g (Const . Just . First))
    phantom

preview :: Getting (Maybe (First x)) xs x -> xs -> Maybe x
preview g = view (pre g)

previews ::
  Getting (Maybe (First x)) xs x -> (Maybe x -> z) -> xs -> z
previews g = views (pre g)

ipre ::
  IxGetting i (Maybe (First (i, x))) xs x ->
  IxPreservingGetter xs (Maybe (i, x))
ipre ig =
  fletch
    ( morphism getFirst
        . getConst
        . ig (Ix \i -> Const . Just . First . (i,))
    )
    phantom

ipreview ::
  IxGetting i (Maybe (First (i, x))) xs x -> xs -> Maybe (i, x)
ipreview ig = view (ipre ig)

ipreviews ::
  IxGetting i (Maybe (First (i, x))) xs x -> xs -> Maybe (i, x)
ipreviews ig = view (ipre ig)

itoListOf :: IxGetting i (Endo [(i, x)]) xs x -> xs -> [(i, x)]
itoListOf ig = ifoldrOf ig (\i x -> ((i, x) :)) []

(^@..) :: xs -> IxGetting i (Endo [(i, x)]) xs x -> [(i, x)]
(^@..) = flip itoListOf

ifoldWithOf :: IxGetting i z xs x -> (i -> x -> z) -> xs -> z
ifoldWithOf = coerce

ifoldrOf ::
  IxGetting i (Endo z) xs x -> (i -> x -> z -> z) -> z -> xs -> z
ifoldrOf ig i_x_z_z z xs =
  getConst (ig ((Const . Endo) #. Ix i_x_z_z) xs) `appEndo` z

ifoldlOf ::
  IxGetting i (Dual (Endo z)) xs x ->
  (i -> z -> x -> z) ->
  z ->
  xs ->
  z
ifoldlOf ig i_z_x_z z xs =
  getDual (ifoldWithOf ig (\i -> (Dual . Endo) #. flip (i_z_x_z i)) xs)
    `appEndo` z

ianyOf :: IxGetting i Any xs x -> (i -> x -> Bool) -> xs -> Bool
ianyOf = coerce

iallOf :: IxGetting i All xs x -> (i -> x -> Bool) -> xs -> Bool
iallOf = coerce

itraverseOf_ ::
  (Along f) =>
  IxGetting i (f z) xs x -> (i -> x -> f z) -> xs -> f ()
itraverseOf_ ig i_x_fz =
  morphism (const ()) . getConst . ig (Ix ((Const .) . i_x_fz))

iforOf_ ::
  (Along f) =>
  IxGetting i (f z) xs x -> xs -> (i -> x -> f z) -> f ()
iforOf_ = flip . itraverseOf_

ifindOf ::
  IxGetting i (Endo (Maybe x)) xs x ->
  (i -> x -> Bool) ->
  xs ->
  Maybe x
ifindOf ig i_x_b =
  ifoldrOf ig (\i x fmx -> if i_x_b i x then Just x else fmx) Nothing

ifindMOf ::
  (Monad f) =>
  IxGetting i (Endo (f (Maybe x))) xs x ->
  (i -> x -> f Bool) ->
  xs ->
  f (Maybe x)
ifindMOf ig i_x_fb =
  ifoldrOf
    ig
    (\i x fmx -> i_x_fb i x >>= \b -> if b then pure (Just x) else fmx)
    (pure Nothing)

(^@?) :: xs -> IxGetting i (Endo (Maybe (i, x))) xs x -> Maybe (i, x)
xs ^@? ig = ifoldrOf ig (\i x _ -> Just (i, x)) Nothing xs

(^@?!) :: xs -> IxGetting i (Endo (i, x)) xs x -> (i, x)
xs ^@?! ig =
  ifoldrOf ig (\i x _ -> (i, x)) (error "(^@?!): empty IxFold") xs

newtype Indexing f x = Indexing {runIndexing :: Natural -> (Natural, f x)}
  deriving (Data.Functor)
instance (Along f) => Morphisms (->) (->) (Indexing f) where
  morphism :: forall x y. (x -> y) -> Indexing f x -> Indexing f y
  morphism x_y (Indexing i_ifx) =
    Indexing (morphism (morphism x_y :: f x -> f y) . i_ifx)
instance (Pure f) => Pure (Indexing f) where
  pure :: x -> Indexing f x
  pure x = Indexing (,pure x)
instance (Apply f) => Apply (Indexing f) where
  (<*>) :: (Apply f) => Indexing f (x -> y) -> Indexing f x -> Indexing f y
  Indexing s0 <*> Indexing s1 = Indexing \i -> case s0 i of
    (j, fxy) -> case s1 j of ~(k, fx) -> (k, fxy <*> fx)
instance (Control.Applicative f) => Control.Applicative (Indexing f) where
  pure :: x -> Indexing f x
  pure x = Indexing (,Control.pure x)
  (<*>) :: Indexing f (x -> y) -> Indexing f x -> Indexing f y
  Indexing s0 <*> Indexing s1 = Indexing \i -> case s0 i of
    (j, fxy) -> case s1 j of ~(k, fx) -> (k, fxy Control.<*> fx)
instance (Against f) => Morphisms Op (->) (Indexing f) where
  morphism :: forall x y. Op x y -> Indexing f x -> Indexing f y
  morphism (Op y_x) (Indexing i_ifx) =
    Indexing (morphism (morphism (Op y_x) :: f x -> f y) . i_ifx)
instance (Semigroup (f x)) => Semigroup (Indexing f x) where
  (<>) :: Indexing f x -> Indexing f x -> Indexing f x
  Indexing s0 <> Indexing s1 = Indexing \i -> case s0 i of
    (j, fx) -> case s1 j of ~(k, fx') -> (k, fx <> fx')
instance (Monoid (f x)) => Monoid (Indexing f x) where
  mempty :: Indexing f x
  mempty = Indexing (,mempty)

indexing ::
  (Ixed Natural p) =>
  Focus (Indexing f) xs ys x y -> Over p f xs ys x y
indexing focus pxfy xs = snd do
  runIndexing (focus (\x -> Indexing \ !i -> (succ i, ixed pxfy i x)) xs) 0

traversed :: (Traversable t) => IxTraversal Natural (t x) (t y) x y
traversed = conjoined traverse (indexing traverse)

traversed1 :: (Traversable1 t) => IxTraversal1 Natural (t x) (t y) x y
traversed1 = conjoined traverse1 (indexing traverse1)

itraversed ::
  forall i t x y. (IxTraversable i t) => IxTraversal i (t x) (t y) x y
itraversed = conjoined traverse (traverse . Ix @i . ixed)

elementsOf ::
  (Applicative f) =>
  Focus (Indexing f) xs ys x x ->
  (Natural -> Bool) ->
  IxFocus Natural f xs ys x x
elementsOf f p pxfy xs = snd $ flip runIndexing 0 do
  flip f xs \x -> Indexing \ !i ->
    (succ i, if p i then ixed pxfy i x else pure x)

elementOf ::
  (Applicative f) =>
  Focus (Indexing f) xs ys x x ->
  Natural ->
  IxFocus Natural f xs ys x x
elementOf f n = elementsOf f (n ==)

element ::
  (Traversable t) => Natural -> IxTraversal Natural (t x) (t x) x x
element n = elementOf traverse n

elements ::
  (Traversable t) => (Natural -> Bool) -> IxTraversal Natural (t x) (t x) x x
elements p = elementsOf traverse p

-- | @
-- type Setter xs ys x y =
--   forall f.
--   (Collectable f, Traversable f, Copure f, Applicative f) =>
--   (x -> f y) -> xs -> f ys
-- @
type Setter xs ys x y =
  forall f.
  (Collectable f, Traversable f, Copure f, Applicative f) =>
  Focus f xs ys x y

-- | @
-- type ASetter xs ys x y = (x -> Identity y) -> xs -> Identity ys
-- @
type ASetter xs ys x y = Focus Identity xs ys x y

-- | @
-- type IxSetter xs ys x y =
--   forall p.
--   (Ixed i p) =>
--   p x (Identity y) -> xs -> Identity ys
-- @
type IxSetter i xs ys x y =
  forall p. (Ixed i p) => Over p Identity xs ys x y

-- | @
-- type AnIxSetter i xs ys x y =
--   Ix i x (Identity y) -> xs -> Identity ys
-- @
type AnIxSetter i xs ys x y = Over (Ix i) Identity xs ys x y

-- | @
-- type IxPreservingSetter xs ys x y =
--   forall p f.
--   ( Conjoined p
--   , Collectable f
--   , Traversable f
--   , Copure f
--   , Applicative f
--   ) =>
--   p x (f y) -> p xs (f ys)
-- @
type IxPreservingSetter xs ys x y =
  forall p f.
  ( Conjoined p
  , Collectable f
  , Traversable f
  , Copure f
  , Applicative f
  ) =>
  Optic p f xs ys x y

-- | @
-- type Setting p xs ys x y = p x (Identity y) -> xs -> Identity ys
-- @
type Setting p xs ys x y = Over p Identity xs ys x y

sets ::
  ( Fletched p
  , Fletched q
  , Collectable f
  , Traversable f
  , Copure f
  , Applicative f
  ) =>
  (p x y -> q xs ys) -> Optical p q f xs ys x y
sets pxy_qxsys pxfy = morphism pure (pxy_qxsys (morphism copure pxfy))

over :: ASetter xs ys x y -> (x -> y) -> xs -> ys
over = coerce

(%~) :: ASetter xs ys x y -> (x -> y) -> xs -> ys
(%~) = over

iover :: AnIxSetter i xs ys x y -> (i -> x -> y) -> xs -> ys
iover = coerce

(%@~) :: AnIxSetter i xs ys x y -> (i -> x -> y) -> xs -> ys
(%@~) = iover

set :: ASetter xs ys x y -> y -> xs -> ys
set s b = runIdentity #. s (\_ -> Identity b)

(.~) :: ASetter xs ys x y -> y -> xs -> ys
(.~) = set

(?~) :: ASetter xs ys x (Maybe y) -> y -> xs -> ys
s ?~ y = s .~ Just y

(+~) :: (Addition x x x) => ASetter xs ys x x -> x -> xs -> ys
s +~ x = over s (+ x)

(-~) :: (Subtraction x x x) => ASetter xs ys x x -> x -> xs -> ys
s -~ x = over s (-. x)

(*~) :: (Multiplication x x x) => ASetter xs ys x x -> x -> xs -> ys
s *~ x = over s (* x)

(/~) :: (Division x x x) => ASetter xs ys x x -> x -> xs -> ys
s /~ x = over s (/ x)

(^~) :: (Power x y x) => ASetter xs ys x x -> y -> xs -> ys
s ^~ y = over s (^ y)

$(Data.traverse fieldN [0 .. 20])
$(Data.traverse (instanceField 0) [0 .. 20])
$(Data.traverse (instanceField 1) [1 .. 20])
$(Data.traverse (instanceField 2) [2 .. 20])
$(Data.traverse (instanceField 3) [3 .. 20])
$(Data.traverse (instanceField 4) [4 .. 20])
$(Data.traverse (instanceField 5) [5 .. 20])
$(Data.traverse (instanceField 6) [6 .. 20])
$(Data.traverse (instanceField 7) [7 .. 20])
$(Data.traverse (instanceField 8) [8 .. 20])
$(Data.traverse (instanceField 9) [9 .. 20])
$(Data.traverse (instanceField 10) [10 .. 20])
$(Data.traverse (instanceField 11) [11 .. 20])
$(Data.traverse (instanceField 12) [12 .. 20])
$(Data.traverse (instanceField 13) [13 .. 20])
$(Data.traverse (instanceField 14) [14 .. 20])
$(Data.traverse (instanceField 15) [15 .. 20])
$(Data.traverse (instanceField 16) [16 .. 20])
$(Data.traverse (instanceField 17) [17 .. 20])
$(Data.traverse (instanceField 18) [18 .. 20])
$(Data.traverse (instanceField 19) [19, 20])
$(Data.traverse (instanceField 20) [20])

instance Field0 (Identity x) (Identity y) x y where
  _0 :: Lens (Identity x) (Identity y) x y
  _0 k (Identity x) = morphism Identity (k x)

class Each xs ys x y | xs -> x, ys -> y, xs y -> ys, ys x -> xs where
  each :: Traversal xs ys x y
  default each ::
    (Traversable t, xs ~ t x, ys ~ t y) =>
    Traversal xs ys x y
  each = traverse

$(Data.traverse instanceEach [0 .. 20])
instance Each (Map k v) (Map k v') v v'
instance Each (IntMap v) (IntMap v') v v'
instance Each (Seq x) (Seq y) x y
