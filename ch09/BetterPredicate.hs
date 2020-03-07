module BetterPredicate where

import Control.Exception
import System.IO (hClose, hFileSize, openFile, IOMode(..))

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
  h  <- openFile path ReadMode
  sz <- hFileSize h
  hClose h
  return sz

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle (\_ -> return Nothing) $ do
  h  <- openFile path ReadMode
  sz <- hFileSize h
  hClose h
  return (Just sz)