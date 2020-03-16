module Supply where

import Control.Monad.State

newtype Supply s a = S (State [s] a)

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

instance Functor (Supply s) where
  fmap f (S s) = S $ fmap f s

instance Applicative (Supply s) where
  pure = S . pure
  (S f) <*> (S s) = S $ f <*> s

instance Monad (Supply s) where
  return = S . return
  s >>= f = S $ unwrapS s >>= (unwrapS . f)

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

next :: Supply s (Maybe s)
next = S $ do
  st <- get
  case st of
    [] -> return Nothing
    (x:xs) -> do put xs
                 return (Just x)
