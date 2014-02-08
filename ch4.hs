
import System.Environment (getArgs)
    

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

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        myFunction = getFirstWords

getFirstWords :: String -> String
getFirstWords "" = ""
getFirstWords ts =
  let (x:xs) = lines ts
  in (head . words $ x) ++ "\n" ++ (getFirstWords (unlines xs))
