import Data.List

splitLines :: String -> [String]
splitLines [] = []
splitLines xs =
  let (pre,suf) = break isReturn xs
  in pre : case suf of
    ('\r':'\n':rest) -> splitLines rest
    ('\r':rest) -> splitLines rest
    ('\n':rest) -> splitLines rest
    _ -> []

isReturn c = c == '\r' || c == '\n'
    
