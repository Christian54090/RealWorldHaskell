module Parse where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy       as L
import Data.Int (Int64)

data ParseState = ParseState {
    string :: L.ByteString
  , offset :: Int64
  } deriving Show

-- best parse
newtype Parse a = Parse { runParse :: ParseState -> Either String (a, ParseState) }

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState =
  case runParse parser (ParseState initState 0) of
    Left err         -> Left err
    Right (result,_) -> Right result

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState { offset = newOffset }

testBefore = ParseState (L8.pack "foo") 0
testAfter  = modifyOffset testBefore 3

modifyOffset' :: ParseState -> Int64 -> ParseState
modifyOffset' (ParseState s _) o = ParseState s o
