module Cup where

-- Constructor: create a cup whose size is floz
cup floz = \message -> message floz

coffeeCup = cup 12

getOz aCup = aCup id

isEmpty aCup = getOz aCup == 0

drink aCup ozDrank =
  if ozDiff >= 0
    then cup ozDiff
    else cup 0
  where
    floz = getOz aCup
    ozDiff = floz - ozDrank

mainCup :: IO ()
mainCup = do
  print $ getOz (drink coffeeCup 10) == 2
  print $ getOz (drink coffeeCup 8) == 4
  print $ getOz (drink coffeeCup 80) == 0
  print $ isEmpty (drink coffeeCup 8) == False
  print $ isEmpty (drink coffeeCup 80) == True
  print $ getOz (foldl drink coffeeCup [1, 1, 1, 1, 1]) == 7
