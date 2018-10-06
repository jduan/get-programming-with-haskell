module Applicative.ListAsAContext where

-- These examples show Lists as nondeterministic computation!
doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxPrize :: [Int]
boxPrize = [500, 20000]

totalPrize :: [Int]
totalPrize = pure (+) <*> doorPrize <*> boxPrize

-- Find all the prime numbers less than n
primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite nums
  where
    nums = [2 .. n]
    composite = pure (*) <*> nums <*> nums
    isNotComposite = not . (`elem` composite)

data User = User
  { name :: String
  , gamerId :: Int
  , score :: Int
  } deriving (Show)

testNames :: [String]
testNames =
  [ "John Smith"
  , "Robert'); DROP TABLE Students;--"
  , "Christina NULL"
  , "Randall Munroe"
  ]

testIds :: [Int]
testIds = [1337, 0123, 999999]

testScores :: [Int]
testScores = [0, 100000, -99999]

-- generate test data that includes all possible combinations of these
-- values. This means nondeterministically computing a list of possible
-- users. You could do that by hand, but that would mean writing out
-- 4 × 3 × 3 = 36 entries! Plus, if you later decide to add another value
-- to any of those lists, it means a lot of work for you.
testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores

-- exercises
allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap func f = pure func <*> f

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6
