module QC where

import Control.Monad (liftM, liftM2)
import Test.QuickCheck
import Prettify2
import System.Random

--instance Arbitrary Char where
--  arbitrary = elements (['A'..'Z'] ++ ['a'..'z'] ++ " ~!@#$%^&*()")

instance Arbitrary Doc where
--  arbitrary = do
--    n <- choose (1,6) :: Gen Int
--    case n of
--      1 -> return Empty
--      2 -> do x <- arbitrary
--              return (Char x)
--      3 -> do x <- arbitrary
--              return (Text x)
--      4 -> return Line
--      5 -> do x <- arbitrary
--              y <- arbitrary
--              return (Concat x y)
--      6 -> do x <- arbitrary
--              y <- arbitrary
--              return (Union x y)
  arbitrary =
    oneof [ return Empty
          , liftM Char arbitrary
          , liftM Text arbitrary
          , return Line
          , liftM2 Concat arbitrary arbitrary
          , liftM2 Union arbitrary arbitrary ]

--gen10 = generate 10 (mkStdGen 2) arbitrary :: [Doc]