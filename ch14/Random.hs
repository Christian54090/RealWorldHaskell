module Random where

import System.Random
import Control.Monad.State

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom =
  get >>= \gen ->
  let (val, gen') = random gen in
  put gen' >>
  return val

getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom

runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
  oldState <- getStdGen
  let (result, newState) = runState getTwoRandoms oldState
  setStdGen newState
  return result

runRandom :: IO Int
runRandom = do
  oldState <- getStdGen
  let (result, newState) = runState getRandom oldState
  setStdGen newState
  return result

getResult :: IO Int
getResult = do
  oldState <- getStdGen
  let (result, _) = runState getRandom oldState
  return result
