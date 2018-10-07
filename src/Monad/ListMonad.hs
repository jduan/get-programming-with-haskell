module Monad.ListMonad where

import Control.Monad (guard)
import Data.Char (toUpper)

-- The first 10 powers of 2
powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (2 ^) [1 .. n]

-- Think of list as a monad
powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  value <- [1 .. n]
  return (2 ^ value)

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  value <- [1 .. n]
  let powersOfTwo = 2 ^ value
  let powersOfThree = 3 ^ value
  return (powersOfTwo, powersOfThree)

-- Generate all possible combinations of odd and even numbers up to n
-- Note that you're working with 2 lists here.
allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  evenValue <- [2,4 .. n]
  oddValue <- [1,3 .. n]
  return (evenValue, oddValue)

squares :: Int -> [(Int, Int)]
squares n = do
  value <- [1 .. n]
  let sq = value ^ 2
  return (value, sq)

evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  -- guard filters out all the values that don't satisfy your test
  guard (even value)
  return value

filter' :: (a -> Bool) -> [a] -> [a]
filter' pred xs = do
  x <- xs
  guard (pred x)
  return x

evenSquares :: Int -> [Int]
evenSquares n = do
  val <- [0 .. n]
  let squared = val ^ 2
  guard (even squared)
  return squared

-- List comprehensions are just specialized applications of list monads!
powersOfTwoAndThree' :: Int -> [(Int, Int)]
powersOfTwoAndThree' n =
  [ (powersOfTwo, powersOfThree)
  | value <- [1 .. n]
  , let powersOfTwo = 2 ^ value
  , let powersOfThree = 3 ^ value
  ]

allEvenOdds' :: Int -> [(Int, Int)]
allEvenOdds' n =
  [(evenValue, oddValue) | evenValue <- [2,4 .. n], oddValue <- [1,3 .. n]]

evensGuard' :: Int -> [Int]
evensGuard' n = [value | value <- [1 .. n], even value]

--
-- exercises
--
capitalizeWords :: [String] -> [String]
capitalizeWords words = ["Mr. " ++ upper word | word <- words]
  where
    upper [] = []
    upper (x:xs) = toUpper x : xs

-- Generate a list of correct calendar dates
calendarDates :: [Int] -> [[Int]]
calendarDates lastDays = [[1 .. lastDay] | lastDay <- lastDays]

calendarDates' :: [Int] -> [[Int]]
calendarDates' lastDays = do
  lastDay <- lastDays
  return [1 .. lastDay]
