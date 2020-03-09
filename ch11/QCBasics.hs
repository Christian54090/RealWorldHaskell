module QCBasics where

import Test.QuickCheck
import Data.List

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = filter (<x) xs
        larger  = filter (>=x) xs

qsort' :: Ord a => [a] -> [a]
qsort' []     = []
qsort' (x:xs) = smaller ++ [x] ++ larger
  where smaller = qsort' $ filter (<x) xs
        larger  = qsort' $ filter (>=x) xs

prop_idempotent xs = qsort (qsort xs) == qsort xs
test = quickCheck (prop_idempotent :: [Integer] -> Bool)

prop_ordered xs = ordered (qsort xs)
  where ordered [] = True
        ordered [x] = True
        ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_maximum xs =
  not (null xs) ==>
      last (qsort xs) == maximum xs