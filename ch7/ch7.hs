

main =
  putStrLn "What is your name?:" >>
  getLine >>=
  putStrLn . ("Hello" ++)
  
