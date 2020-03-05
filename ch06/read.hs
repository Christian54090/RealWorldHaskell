
main :: IO ()
main = do
  putStrLn "Please enter a Double:"
  inpStr <- getLine
  let inpDouble = (read inpStr)::Double
  putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show (inpDouble * 2))

readFive = (read "5")::Int

data Color = Red | Green | Blue deriving (Show, Eq)

instance Read Color where
  readsPrec _ value = tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
    where tryParse :: [(String, Color)] -> [(Color, String)]
          tryParse [] = []
          tryParse ((att, res):xs) =
            if (take (length att) value) == att
            then [(res, drop (length att) value)]
            else tryParse xs