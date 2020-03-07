module SimpleFinder where

import RecursiveContents
import System.FilePath (takeExtension)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
  names <- getRecursiveContents path
  return (filter p names)

simpleFindEx :: FilePath -> IO [FilePath]
simpleFindEx = simpleFind (\p -> takeExtension p == ".c")