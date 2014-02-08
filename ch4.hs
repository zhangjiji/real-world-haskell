
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail [] = Nothing
safeTail xs = Just (tail xs)

safeInit [] = Nothing
safeInit xs = Just (init xs)

safeLast [] = Nothing
safeLast xs = Just (last xs)


splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs =
  let (h,t) = span p xs
  in if null h
     then [head t] : splitWith p (tail t)
     else h:(splitWith p t)
