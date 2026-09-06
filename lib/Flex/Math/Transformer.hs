{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Flex.Math.Transformer where

import Flex.Math.Category
import Flex.Math.Category qualified as Flex

import Data.Either
import Data.Function (($))
import Data.Functor.Identity
import Data.Maybe

class (forall m. (Monad m) => Monad (t m)) => MonadTrans t where
  lift :: (Monad m) => m x -> t m x

instance MonadTrans (StateT s) where
  lift :: (Monad m) => m x -> StateT s m x
  lift mx = StateT \s -> mx >>= \x -> pure (s, x)

class (Monad m) => MonadState s m | m -> s where
  gets :: (s -> x) -> m x
  put :: s -> m ()
  modify :: (s -> s) -> m ()

get :: (MonadState s m) => m s
get = gets id

instance (Monad m) => MonadState s (StateT s m) where
  gets :: (s -> x) -> StateT s m x
  gets s_x = StateT \s -> pure (s, s_x s)
  put :: s -> StateT s m ()
  put s = StateT \_ -> pure (s, ())
  modify :: (s -> s) -> StateT s m ()
  modify s_s = StateT \s -> pure (s_s s, ())

newtype EitherT e m x = EitherT {runEitherT :: m (Either e x)}
instance (Monad m) => Morphisms (->) (->) (EitherT e m) where
  morphism :: forall x y. (x -> y) -> EitherT e m x -> EitherT e m y
  morphism x_y (EitherT meex) = EitherT Flex.do
    morphism (morphism x_y :: Either e x -> Either e y) meex
instance (Monad m) => Pure (EitherT e m) where
  pure :: x -> EitherT e m x
  pure x = EitherT (pure (Right x))
instance (Monad m) => Apply (EitherT e m) where
  (<*>) :: EitherT e m (x -> y) -> EitherT e m x -> EitherT e m y
  EitherT mxy <*> EitherT mx = EitherT Flex.do
    x_y <- mxy
    x <- mx
    pure (x_y <*> x)
instance (Monad m) => Bind (EitherT e m) where
  (>>=) :: EitherT e m x -> (x -> EitherT e m y) -> EitherT e m y
  EitherT meex >>= f = EitherT Flex.do
    eex <- meex
    case eex of
      Left e -> pure (Left e)
      Right x -> (f x).runEitherT
instance MonadTrans (EitherT e) where
  lift :: (Monad m) => m x -> EitherT e m x
  lift = EitherT . morphism Right

class (Monad m) => MonadEither e m | m -> e where
  throw :: e -> m x
  catch :: m x -> (e -> m x) -> m x

instance (Monad m) => MonadEither e (EitherT e m) where
  throw :: e -> EitherT e m x
  throw e = EitherT (pure (Left e))
  catch :: EitherT e m x -> (e -> EitherT e m x) -> EitherT e m x
  catch (EitherT meex) e_mx = EitherT Flex.do
    eex <- meex
    case eex of
      Left e -> (e_mx e).runEitherT
      Right x -> pure (Right x)

newtype EitherStateT e s m x = EitherStateT
  {runEitherStateT :: s -> m (s, Either e x)}
type EitherState e s x = EitherStateT e s Identity x
runEitherState :: EitherState e s x -> s -> (s, Either e x)
runEitherState = (runIdentity .) . runEitherStateT

instance (Monad m) => Morphisms (->) (->) (EitherStateT e s m) where
  morphism ::
    (Monad m) => (x -> y) -> EitherStateT e s m x -> EitherStateT e s m y
  morphism x_y (EitherStateT s_mseex) = EitherStateT \s -> Flex.do
    (s', eex) <- s_mseex s
    pure (s', morphism x_y eex)
instance (Monad m) => Pure (EitherStateT e s m) where
  pure :: x -> EitherStateT e s m x
  pure x = EitherStateT \s -> pure (s, Right x)
instance (Monad m) => Apply (EitherStateT e s m) where
  (<*>) ::
    EitherStateT e s m (x -> y) -> EitherStateT e s m x -> EitherStateT e s m y
  EitherStateT mxy <*> EitherStateT mx = EitherStateT \s -> Flex.do
    (s', eexy) <- mxy s
    case eexy of
      Left e -> pure (s', Left e)
      Right x_y -> Flex.do
        (s'', eex) <- mx s'
        pure (s'', morphism x_y eex)
instance (Monad m) => Bind (EitherStateT e s m) where
  (>>=) ::
    EitherStateT e s m x -> (x -> EitherStateT e s m y) -> EitherStateT e s m y
  EitherStateT mx >>= f = EitherStateT \s -> Flex.do
    (s', eex) <- mx s
    case eex of
      Left e -> pure (s', Left e)
      Right x -> (f x).runEitherStateT s'
instance MonadTrans (EitherStateT e s) where
  lift :: (Monad m) => m x -> EitherStateT e s m x
  lift mx = EitherStateT \s -> mx >>= \x -> pure (s, Right x)
instance (Monad m) => MonadEither e (EitherStateT e s m) where
  throw :: e -> EitherStateT e s m x
  throw e = EitherStateT \s -> pure (s, Left e)
  catch ::
    EitherStateT e s m x ->
    (e -> EitherStateT e s m x) ->
    EitherStateT e s m x
  catch (EitherStateT s_mseex) e_mx = EitherStateT \s -> Flex.do
    (s', eex) <- s_mseex s
    case eex of
      Left e -> (e_mx e).runEitherStateT s'
      Right x -> pure (s', Right x)
instance (Monad m) => MonadState s (EitherStateT e s m) where
  gets :: (s -> x) -> EitherStateT e s m x
  gets s_x = EitherStateT \s -> pure (s, Right (s_x s))
  put :: s -> EitherStateT e s m ()
  put s = EitherStateT \_ -> pure (s, Right ())
  modify :: (s -> s) -> EitherStateT e s m ()
  modify s_s = EitherStateT \s -> pure (s_s s, Right ())

newtype MaybeT m x = MaybeT {runMaybeT :: m (Maybe x)}
hoistMaybe :: (Monad m) => Maybe x -> MaybeT m x
hoistMaybe mx = MaybeT (pure mx)

fromEitherT :: (Monad m) => EitherT e m x -> MaybeT m x
fromEitherT (EitherT mx) = MaybeT Flex.do
  eex <- mx
  pure case eex of
    Left _ -> Nothing
    Right x -> Just x

instance (Monad m) => Morphisms (->) (->) (MaybeT m) where
  morphism :: forall x y. (x -> y) -> MaybeT m x -> MaybeT m y
  morphism x_y (MaybeT mx) = MaybeT (morphism (morphism x_y :: Maybe x -> Maybe y) mx)
instance (Monad m) => Pure (MaybeT m) where
  pure :: x -> MaybeT m x
  pure x = MaybeT (pure (Just x))
instance (Monad m) => Apply (MaybeT m) where
  (<*>) :: MaybeT m (x -> y) -> MaybeT m x -> MaybeT m y
  MaybeT mxy <*> MaybeT mx = MaybeT Flex.do
    mxy >>= \case
      Nothing -> pure Nothing
      Just x_y -> mx >>= \x -> pure (morphism x_y x)
instance (Monad m) => Bind (MaybeT m) where
  (>>=) :: MaybeT m x -> (x -> MaybeT m y) -> MaybeT m y
  MaybeT mmx >>= f = MaybeT Flex.do
    mmx >>= \case
      Nothing -> pure Nothing
      Just x -> (f x).runMaybeT
instance MonadTrans MaybeT where
  lift :: (Monad m) => m x -> MaybeT m x
  lift mx = MaybeT (morphism Just mx)
instance (Monad m) => MonadEither () (MaybeT m) where
  throw :: () -> MaybeT m x
  throw () = hoistMaybe Nothing
  catch :: MaybeT m x -> (() -> MaybeT m x) -> MaybeT m x
  catch (MaybeT mmx) u_mmx = MaybeT Flex.do
    mmx >>= \case
      Nothing -> (u_mmx ()).runMaybeT
      Just x -> pure (Just x)

newtype ContT r m x = ContT {runContT :: (x -> m r) -> m r}
evalContT :: (Pure m) => ContT r m r -> m r
evalContT (ContT rmr_mr) = rmr_mr pure

reset :: (Monad m) => ContT r m r -> ContT r' m r
reset = lift . evalContT

shift :: (Monad m) => ((a -> m r) -> ContT r m r) -> ContT r m a
shift f = ContT (evalContT . f)

type Cont r x = ContT r Identity x
runCont :: Cont r x -> (x -> r) -> r
runCont (ContT x_ir) x_r = runIdentity (x_ir (Identity . x_r))
evalCont :: Cont r r -> r
evalCont (ContT r) = runIdentity (r Identity)

instance (Monad m) => Morphisms (->) (->) (ContT r m) where
  morphism :: (x -> y) -> ContT r m x -> ContT r m y
  morphism x_y (ContT xmr_mr) = ContT \y_mr -> xmr_mr (y_mr . x_y)
instance (Monad m) => Pure (ContT r m) where
  pure :: x -> ContT r m x
  pure x = ContT ($ x)
instance (Monad m) => Apply (ContT r m) where
  (<*>) :: ContT r m (x -> y) -> ContT r m x -> ContT r m y
  ContT cxy <*> ContT cx = ContT \k0 -> cxy \x_y -> cx \x -> k0 (x_y x)
  liftA2 :: (x -> y -> z) -> ContT r m x -> ContT r m y -> ContT r m z
  liftA2 f (ContT cx) (ContT cy) = ContT \k0 -> cx \x -> cy \y -> k0 (f x y)
instance (Monad m) => Bind (ContT r m) where
  (>>=) :: ContT r m x -> (x -> ContT r m y) -> ContT r m y
  (ContT cx) >>= f = ContT \k0 -> cx \x -> (f x).runContT k0
instance MonadTrans (ContT r) where
  lift :: (Monad m) => m x -> ContT r m x
  lift mx = ContT (mx >>=)

liftCC ::
  forall t m x y.
  (MonadTrans t, Monad m) =>
  (((t m x -> m y) -> m (t m x)) -> m (t m x)) ->
  (((x -> t m y) -> t m x) -> t m x)
liftCC f g = (join . lift . f) \exit -> pure (g (lift . exit . pure))

class (Monad m) => MonadCont m where
  cc :: ((x -> m y) -> m x) -> m x
instance (Monad m) => MonadCont (ContT r m) where
  cc :: ((x -> ContT r m y) -> ContT r m x) -> ContT r m x
  cc f = ContT \k0 -> (f \x -> ContT \_ -> k0 x).runContT k0
instance (MonadCont m) => MonadCont (EitherT e m) where
  cc :: ((x -> EitherT e m y) -> EitherT e m x) -> EitherT e m x
  cc = liftCC cc
instance (MonadCont m) => MonadCont (StateT s m) where
  cc :: ((x -> StateT s m y) -> StateT s m x) -> StateT s m x
  cc = liftCC cc
instance (MonadCont m) => MonadCont (EitherStateT e s m) where
  cc :: ((x -> EitherStateT e s m y) -> EitherStateT e s m x) -> EitherStateT e s m x
  cc = liftCC cc
