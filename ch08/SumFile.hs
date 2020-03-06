module SumFile where

-- reads a text file full of numbers and prints their sum
sumFile = do
  contents <- getContents
  print (sumFile contents)
  where sumFile = sum . map read . words