myDrop :: Int -> [a] -> [a]
myDrop 0 xs     = xs
myDrop _ []     = []
myDrop n (x:xs) = myDrop (n-1) xs

lastButOne :: [a] -> a
lastButOne []       = error "empty list"
lastButOne (_:[])   = error "list too short"
lastButOne (x:_:[]) = x
lastButOne (x:xs)   = lastButOne xs