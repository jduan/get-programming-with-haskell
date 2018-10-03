module IO.Hello where

import qualified Data.Map as Map

-- This is a pure function!
helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

mainHello :: IO ()
mainHello = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

mapData :: Map.Map String String
mapData = Map.fromList [("name", "Jingjing")]

maybeHello :: Maybe String
maybeHello = do
  name <- Map.lookup "name" mapData
  return (helloPerson name)
