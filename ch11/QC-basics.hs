import Test.QuickCheck
import Data.List

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where lhs = filter (< x) xs
        rhs = filter (>= x) xs

prop_indempotent xs = qsort (qsort xs) == qsort xs

prop_mininum xs = not (null xs) ==> head (qsort xs) == minimum xs

prop_ordered xs = ordered (qsort xs)
  where ordered [] = True
        ordered [x] = True
        ordered (x1:x2:xs) = x1 <= x2 && ordered (x2:xs)

prop_permutation xs = permutation xs (qsort xs)
  where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_maximum xs = not (null xs) ==> last (qsort xs) == maximum xs

prop_append xs ys =
  not (null xs) ==> not (null ys) ==>
  head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)

prop_model_sort xs = sort xs == qsort xs
