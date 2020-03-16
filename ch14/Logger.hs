module Logger where

type Log = [String]

ioTest :: IO ()
ioTest = getLine >>= (putStrLn . (++" world"))

myLiftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
myLiftM2 f m1 m2 =
  m1 >>= \a ->
  m2 >>= \b ->
  return (f a b)

newtype Logger a = Logger { execLogger :: (a, Log) }

record s = Logger ((),[s])
--
--instance Monad Logger where
--  return a = Logger (a,[])
--  m >>= f = let (a,w) = execLogger m
--                n     = f a
--                (b,x) = execLogger n
--            in Logger (b, w ++ x)