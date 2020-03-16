module HandleIO where

import System.IO (Handle, IOMode(..))
import qualified System.IO

newtype HandleIO a = HandleIO { runHandleIO :: IO a }

instance Functor HandleIO where
  fmap f h = HandleIO $ fmap f (runHandleIO h)

instance Applicative HandleIO where
  pure = HandleIO . pure
  (HandleIO f) <*> (HandleIO h) = HandleIO $ f <*> h

instance Monad HandleIO where
  return = pure
  (HandleIO h) >>= f = HandleIO $ h >>= (runHandleIO . f)

openFile :: FilePath -> IOMode -> HandleIO Handle
openFile path mode = HandleIO (System.IO.openFile path mode)

hClose :: Handle -> HandleIO ()
hClose = HandleIO . System.IO.hClose

hPutStrLn :: Handle -> String -> HandleIO ()
hPutStrLn h s = HandleIO (System.IO.hPutStrLn h s)

safeHello :: FilePath -> HandleIO ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h
