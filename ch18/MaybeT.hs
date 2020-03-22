{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}

module MaybeT where

import Control.Applicative (liftA2)
import Control.Monad.State
import Control.Monad.Trans

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
bindMT m f = MaybeT $ do
  unwrapped <- runMaybeT m
  case unwrapped of
    Nothing -> return Nothing
    Just y  -> runMaybeT (f y)

bindMT' m f = MaybeT $ runMaybeT m >>= maybe (return Nothing) (runMaybeT . f)

returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

instance Functor m => Functor (MaybeT m) where
  fmap f m = MaybeT $ (fmap . fmap) f (runMaybeT m)

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  f <*> m = MaybeT $ liftA2 (<*>) (runMaybeT f) (runMaybeT m)

instance Monad m => Monad (MaybeT m) where
  return = pure
  fail = failMT
  (>>=) = bindMT

instance MonadTrans MaybeT where
  lift m = MaybeT (Just `liftM` m)

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (MaybeT m) where
  get = lift get
  put = lift . put
