import System.Environment (getArgs)
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
    
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        myFunction = fixLines

fixLines :: String -> String
fixLines = unlines . splitLines
