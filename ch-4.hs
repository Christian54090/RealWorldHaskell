import Data.Monoid
import Data.Char

data a `Pair` b = a `Pair` b deriving Show
data Pair2 a b = Pair2 a b deriving Show

myDumbExample :: [Char] -> Char
myDumbExample xs = if length xs > 0
                   then head xs
                   else 'Z'

mySmartExample :: [Char] -> Char
mySmartExample xs = if not $ null xs
                    then head xs
                    else 'Z'

myOtherExample :: [Char] -> Char
myOtherExample []    = 'Z'
myOtherExample (x:_) = x

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:[]) = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs

safeLast' :: [a] -> Maybe a
safeLast' [] = Nothing
safeLast' xs = Just $ xs !! (length xs - 1)

safeInit :: [a] -> Maybe [a]
safeInit []     = Nothing
safeInit (x:[]) = Just []
safeInit (x:xs) = Just [x] <> safeInit xs

safeInit' :: [a] -> Maybe [a]
safeInit' [] = Nothing
safeInit' xs = Just $ (reverse $ tail $ reverse xs)

-- (.&.) is bitwise and
-- (.|.) is bitwise or

asInt :: String -> Int
asInt s = read s :: Int

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs)
  | f x = x : takeWhile' f xs
  | otherwise = []