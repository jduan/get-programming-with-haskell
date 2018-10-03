module IO.SumLazy where

toInts :: String -> [Int]
toInts = map read . lines

mainSumLazy :: IO ()
mainSumLazy = do
  userInput <- getContents
  let numbers = toInts userInput
  let squares = map (^ 2) numbers
  print (sum squares)
