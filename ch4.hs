
import System.Environment (getArgs)
import Data.Char    

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

transposeList [] = []
transposeList (x:xs) = [x]: (transposeList xs)

transposeLists [] = []
transposeLists (x:[]) = transposeList x
transposeLists (x:xs) = zipWith (++) (transposeList x) (transposeLists xs)

transposeFile :: String -> String
transposeFile = unlines . transposeLists . lines

myMap _ [] = []
myMap f xs = foldr step [] xs
  where step y acc = f y : acc

-- headache pills
foldl'' f z xs = foldr step id xs z
  where step x g a = g (f a x)

{-
foldr _ [] = []
foldr f z (x:xs) = f x : (foldr f z xs)

foldl'' f z [x1,x2] -->
step x1 (step x2 (step id [])) -->
step x1 id (f z x2) -->
id (f (f z x2) x1)
        acc
-}

asInt :: String -> Int
asInt "" = error "empty"
asInt "-" = error "only minus?"
asInt ('-':ss) = (-(asInt ss))
asInt ss
  | '.' `elem` ss = error "dot"
  | length ss > 20 = error "too long"
  | otherwise = foldl step 0 ss
  where step acc x = (digitToInt x) + acc * 10

asInt' :: String -> Either String Int
asInt' "" = Left "empty"
asInt' "-" = Left "only minus?"
asInt' ('-':ss) = Right (-(asInt ss))
asInt' ss
  | '.' `elem` ss = Left "dot"
  | length ss > 20 = Left "too long"
  | not (all isDigit ss) = Left "not all digit"
  | otherwise = Right (foldl step 0 ss)
  where step acc x = (digitToInt x) + acc * 10

concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p xs = foldr step [] xs
  where step x acc
          | p x = x:acc
          | otherwise = []

takeWhileDirect _ [] = []
takeWhileDirect p (x:xs)
  | p x = x: takeWhileDirect p xs
  | otherwise = []

groupBy' p xs = foldr step [[]] xs
  where step x [[]] = [[x]]
        step x as'@(a:as) =
          let h = head a
          in if p x h
             then [x:a]
             else [x]:as'

