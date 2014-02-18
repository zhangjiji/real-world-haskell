
foldA :: Ix k => (a -> b -> c) -> a -> Array k b -> a
foldA f s a = go s (indices a)
  where go s (j:js) = let s' = f s (a ! j)
                             in s' `seq` go s' js
           go s _ = s
