import Control.Monad
--import Control.Applicative((<*>))
--import Data.Functor((<$>))

data MovieReview = MovieReview {
  revTitle :: String
  , revUser :: String
  , revReview :: String
    } deriving (Show)

liftedReview alist =
  liftM3 MovieReview (lookup1 "title" alist) (lookup1 "user" alist) (lookup1 "review" alist)

lookup1 key alist = case lookup key alist of
  Just (Just s@(_:_)) -> Just s
  _ -> Nothing

r1 = [("title", Nothing), ("user", Just "2"), ("review", Just "3")]
r2 = [("title",Just "1"),("user",Just "2"),("review",Just "3")]

apReview alist =
  MovieReview `liftM` lookup1 "title" alist
  `ap` lookup1 "user" alist
  `ap` lookup1 "review" alist
--  <*> lookup1 "user" alist
--  <*> lookup1 "review" alist
