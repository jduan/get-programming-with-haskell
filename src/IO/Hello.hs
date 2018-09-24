module Hello where

-- This is a pure function!
helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

mainHello :: IO ()
mainHello = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement
