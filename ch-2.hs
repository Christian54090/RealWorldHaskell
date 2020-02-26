myDrop :: Int -> [a] -> [a]
myDrop 0 xs     = xs
myDrop _ []     = []
myDrop n (x:xs) = myDrop (n-1) xs