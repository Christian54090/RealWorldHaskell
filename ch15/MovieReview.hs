module MovieReview where

import Control.Monad

data MovieReview = MovieReview {
    revTitle :: String
  , revUser :: String
  , revReview :: String
  } deriving Show

simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview alist =
  case lookup "title" alist of
    Just (Just title@(_:_)) ->
      case lookup "user" alist of
        Just (Just user@(_:_)) ->
          case lookup "review" alist of
            Just (Just review@(_:_)) ->
              Just (MovieReview title user review)
            _ -> Nothing -- no review
        _ -> Nothing -- no user
    _ -> Nothing -- no title

lookup1 :: String -> [(String, Maybe String)] -> Maybe String
lookup1 key alist =
  case lookup key alist of
    Just (Just s@(_:_)) -> Just s
    _ -> Nothing

maybeReview :: [(String, Maybe String)] -> Maybe MovieReview
maybeReview alist = do
  title <- lookup1 "title" alist
  user <- lookup1 "user" alist
  review <- lookup1 "review" alist
  return (MovieReview title user review)

liftedReview alist =
  liftM3 MovieReview (lookup1 "title" alist)
                     (lookup1 "user" alist)
                     (lookup1 "review" alist)

apReview alist =
  MovieReview `liftM` lookup1 "title" alist
                 `ap` lookup1 "user" alist
                 `ap` lookup1 "review" alist

-- ap is just the monad $
-- ($) :: (a -> b) -> a -> b
-- ap :: Monad m => m (a -> b) -> m a -> m b
