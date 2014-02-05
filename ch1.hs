-- file: ch01/WC.hs

-- main = interact wordCount
--   where wordCount input = show (length (lines input)) ++ "\n"

main = interact wordCount
  where wordCount input = show (length input) ++ "\n"
