{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MaybeTParse where

import MaybeT
import Control.Monad.State
import Control.Monad.Trans
import Data.Int (Int64)
import Control.Applicative (liftA2)
import qualified Data.ByteString.Lazy as L

data ParseState = ParseState {
    string :: L.ByteString
  , offset :: Int64
  } deriving (Show)

--newtype Parse a = P {
--    runP :: MaybeT (State ParseState) a
--  } deriving (Monad, MonadState ParseState)
--
--evalParse :: Parse a -> L.ByteString -> Maybe a
--evalParse m s = evalState (runMaybeT (runP m)) (ParseState s 0)

type ParseError = String

newtype EitherT m a b = EitherT { runEitherT :: m (Either a b) }

instance Functor m => Functor (EitherT m a) where
  fmap f e = EitherT $ (fmap . fmap) f (runEitherT e)

instance Applicative m => Applicative (EitherT m a) where
  pure = EitherT . pure . Right
  f <*> e = EitherT $ liftA2 (<*>) (runEitherT f) (runEitherT e)

instance Monad m => Monad (EitherT m a) where
  return = pure
  e >>= f = EitherT $ do
    unwrapped <- runEitherT e
    case unwrapped of
      Left a -> return $ Left a
      Right a -> runEitherT (f a)

instance MonadTrans (EitherT m) where
  lift = EitherT $ runEitherT . lift