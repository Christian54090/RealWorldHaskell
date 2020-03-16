module RandomSupply where

import Supply
import System.Random hiding (next)
import Control.Arrow (first)

randomsIO :: Random a => IO [a]
randomsIO = getStdRandom $ \g ->
  let (a, b) = split g
  in (randoms a, b)

randomsIO_golfed :: Random a => IO [a]
randomsIO_golfed = getStdRandom (first randoms . split)

takeIO :: Int -> IO [a] -> IO [a]
takeIO n ioList = do
  list <- ioList
  return $ take n list

takeIO' :: Int -> IO [a] -> IO [a]
takeIO' n = flip (>>=) (return . take n)