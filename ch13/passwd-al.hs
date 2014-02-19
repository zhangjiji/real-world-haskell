
import Data.List
import System.IO
import Control.Monad(when)
import System.Exit
import System.Environment(getArgs)

main = do
  args <- getArgs

  when (length args /= 2) $ do
    putStrLn "Syntax: passwd-al filename uid"
    exitFailure

  content <- readFile (args !! 0)

  let userName = findByUID content (read (args !! 1))

  case userName of
    Just x -> putStrLn x
    Nothing -> putStrLn "Could not find that UID"

findByUID :: String -> Integer -> Maybe String
findByUID content uid =
  let al = map parseLine . lines $ content
  in lookup uid al

parseLine :: String -> (Integer, String)
parseLine input =
  let fields = split' ':' input
  in (read (fields !! 2), fields !! 0)

split' :: (Eq a) => a -> [a] -> [[a]]
split' _ [] = [[]]
split' delim str =
  let (before, remainder) = span (/= delim) str
  in before : case remainder of
    [] -> []
    x -> split' delim (tail x)
