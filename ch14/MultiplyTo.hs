guarded :: Bool -> [a] -> [a]
guarded True xs = xs
guarded False _ = []

multiplyTo :: Int -> [(Int, Int)]
multiplyTo n = do
  x <- [1..n]
  y <- [x..n]
  guarded (x * y == n) $ return (x, y)

robust :: [a] -> Maybe a
robust xs = do
  (_:x:_) <- Just xs
  return x

robust_desugar :: [a] -> Maybe a
robust_desugar xs =
  let f (_:x:_) = do
        return x
  in Just xs >>= f
