module PasswdA1 where

import Data.List
import System.IO
import Control.Monad (when)
import System.Exit
import System.Environment (getArgs)

passwdMain = do
  args <- getArgs

  when (length args /= 2) $ do
    putStrLn "Syntax: passwd-a1 filename uid"
    exitFailure

  content <- readFile (args !! 0)
  let  username = findByUID content (read $ args !! 1)
  case username of
    Just x  -> putStrLn x
    Nothing -> putStrLn "Could not find that UID"

findByUID :: String -> Integer -> Maybe String
findByUID content uid =
  let a1 = map parseline . lines $ content
      in lookup uid a1

parseline :: String -> (Integer, String)
parseline input =
  let fields = split ':' input
      in (read (fields !! 2), fields !! 0)

split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split delim str =
  -- find the part of the list before the delimiter and put it in 'before'.
  -- rest of the list, including the delimiter, goes in 'remainder'.
  let (before, remainder) = span (/=delim) str
      in
      before : case remainder of
                    [] -> []
                    x  -> split delim (tail x) -- if more data to process, recurse
