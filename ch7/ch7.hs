

main = do
  putStrLn "What is your name?:"
  str <- getLine
  putStrLn $ "Hello" ++ str
  
