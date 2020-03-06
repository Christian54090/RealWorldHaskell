module Glob where

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents )
import Control.Exception (handle)
import Control.Monad (forM)
