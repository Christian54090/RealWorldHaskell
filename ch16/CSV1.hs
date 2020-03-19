module CSV1 where

import Text.ParserCombinators.Parsec

csvFile :: GenParser Char st [[String]]
csvFile = do
  result <- many line
  eof                   -- end of file
  return result

-- each line contains 1 or more cells, separated by a comma
line :: GenParser Char st [String]
line = do
  result <- cells
  eol                   -- end of line
  return result

-- build up a list of cells. try to parse 1st cell, then figure out what ends cell
cells :: GenParser Char st [String]
cells = do
  first <- cellContent
  next <- remainingCells
  return (first : next)

-- the cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesnt, indicating that we're at the end of the cells for this line
remainingCells :: GenParser Char st [String]
remainingCells = (char ',' >> cells) <|> (return [])

cellContent :: GenParser Char st String
cellContent = many (noneOf ",\n")

eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
