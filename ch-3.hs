import Data.List

data BookInfo = Book Int String [String] deriving (Show)
-- BookInfo is the name of our new type. we call BookInfo a 'type constructor'
-- Book is the name of the 'value' constructor' or 'data constructor'
-- Int, String, and [String] are the components of the type (often referred to as 'fields')

data MagazineInfo = Magazine Int String [String] deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

data BookReview = BookReview BookInfo CustomerID String

type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

data Customer = Customer {
    customerID :: CustomerID
  , name :: String
  , address :: [String]
  } deriving (Show)

data List a = Nil | Cons a (List a) deriving (Show)

list = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

myLen :: [a] -> Int
myLen [] = 0
myLen (x:xs) = 1 + myLen xs

mean :: Integral a => [a] -> Double
mean xs = (fromIntegral $ sum xs) / (fromIntegral $ myLen xs)

toPalindrome :: [a] -> [a]
toPalindrome xs = xs ++ reverse xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

intersperse' :: a -> [[a]] -> [a]
intersperse' a [] = []
intersperse' a (xs:[]) = xs
intersperse' a (xs:rest) = xs ++ [a] ++ intersperse' a rest

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show)

tree =
  Node (Node Empty 0 Empty)
       1
       (Node Empty 2 Empty)

tree2 =
  Node (Node Empty 3 Empty)
       4
       (Node Empty 5 Empty)

instance Functor Tree where
  fmap _ Empty          = Empty
  fmap f (Node xs y zs) = Node (fmap f xs) (f y) (fmap f zs)

zipTree :: Ord a => Tree a -> Tree a -> Tree a
zipTree a Empty = a
zipTree Empty a = a
zipTree (Node as b cs) (Node xs y zs)
  | b > y     = Node (zipTree as (Node xs y zs)) b cs
  | otherwise = Node as b (zipTree cs (Node xs y zs))