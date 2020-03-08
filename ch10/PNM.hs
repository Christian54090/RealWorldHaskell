module PNM where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy       as L
import Data.Char (isSpace)

data GreyMap = GreyMap {
    greyWidth :: Int
  , greyHeight :: Int
  , greyMax :: Int
  , greyData :: L.ByteString
  } deriving Eq

instance Show GreyMap where
  show (GreyMap w h m _) = "GreyMap " ++ show w ++ "x" ++ show h ++ " " ++ show m

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader pre str
  | pre `L8.isPrefixOf` str = Just $ L8.dropWhile isSpace (L.drop (L.length pre) str)
  | otherwise               = Nothing

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s =
  case L8.readInt s of
    Nothing -> Nothing
    Just (num, rest)
        | num <= 0 -> Nothing
        | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str =
  let count = fromIntegral n
      both@(prefix,_) = L.splitAt count str
  in if L.length prefix < count
     then Nothing
     else Just both

parseP5 :: L.ByteString -> Maybe (GreyMap, L.ByteString)
parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 ->
      case getNat s1 of
        Nothing      -> Nothing
        Just (w, s2) ->
          case getNat (L8.dropWhile isSpace s2) of
            Nothing      -> Nothing
            Just (h, s3) ->
              case getNat (L8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (m, s4)
                  | m > 255   -> Nothing
                  | otherwise ->
                      case getBytes 1 s4 of
                        Nothing      -> Nothing
                        Just (_, s5) ->
                          case getBytes (w*h) s5 of
                            Nothing -> Nothing
                            Just (bitmap, s6) ->
                              Just (GreyMap w h m bitmap, s6)

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a,s) = Just (a, L8.dropWhile isSpace s)

parseP5_take2 :: L.ByteString -> Maybe (GreyMap, L.ByteString)
parseP5_take2 s =
  matchHeader (L8.pack "P5") s >>?
  \s -> skipSpace ((), s)      >>?
  (getNat . snd)               >>?
  skipSpace                    >>?
  \(w,s) -> getNat s           >>?
  skipSpace                    >>?
  \(h,s) -> getNat s           >>?
  \(m,s) -> getNat s           >>?
  (getBytes (w * h) . snd)     >>?
  \(bitmap,s) -> Just (GreyMap w h m bitmap, s)