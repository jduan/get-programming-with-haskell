module Main where

import Applicative.ApplicativeExercises

main :: IO ()
main = do
  putStrLn "Enter name of the first city"
  name1 <- getLine
  putStrLn "Enter name of the second city"
  name2 <- getLine
  let dist = calculateDistance name1 name2
  case dist of
    Nothing -> putStrLn "Couldn'd find one of the cities or both!"
    Just d -> putStrLn $ "Distance is " ++ show d
