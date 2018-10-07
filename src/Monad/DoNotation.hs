module Monad.DoNotation where

import qualified Data.Map as Map

maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM mp = mp >>= (\(x, y) -> return $ max x y)

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloNameDo :: IO ()
helloNameDo = do
  askForName
  name <- getLine
  putStrLn (nameStatement name)

echo :: IO ()
echo = getLine >>= putStrLn

echoDo :: IO ()
echoDo = do
  line <- getLine
  putStrLn line

-- Problem: interview candidates.
-- You want to determine whether candidates passed or failed your
-- interview process. You'll see how the same code can handle candidates in
-- the context of IO, Maybe, and even List. In the end, you'll be able to
-- refactor the code you've reused in each section to a single function
-- that works on all instances of Monad.
--
data Grade
  = F
  | D
  | C
  | B
  | A
  deriving (Eq, Ord, Enum, Show, Read)

data Degree
  = HS
  | BA
  | MS
  | PhD
  deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
  { candidateId :: Int
  , codeReview :: Grade
  , cultureFit :: Grade
  , education :: Degree
  } deriving (Show, Eq)

-- check if a candidate passes minimal requirements
viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where
    passedCoding = codeReview candidate > B
    passedCultureFit = cultureFit candidate > C
    educationMin = education candidate >= MS
    tests = [passedCoding, passedCultureFit, educationMin]

--
-- The IO context
--
readInt :: IO Int
readInt = read <$> getLine

readGrade :: IO Grade
readGrade = read <$> getLine

readDegree :: IO Degree
readDegree = read <$> getLine

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "enter id:"
  cId <- readInt
  putStrLn "enter code grade:"
  codeGrade <- readGrade
  putStrLn "enter culture fit grade:"
  cultureGrade <- readGrade
  putStrLn "enter education:"
  degree <- readDegree
  return $
    Candidate
      { candidateId = cId
      , codeReview = codeGrade
      , cultureFit = cultureGrade
      , education = degree
      }

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

candidate1 :: Candidate
candidate1 =
  Candidate {candidateId = 1, codeReview = A, cultureFit = A, education = BA}

candidate2 :: Candidate
candidate2 =
  Candidate {candidateId = 2, codeReview = C, cultureFit = A, education = PhD}

candidate3 :: Candidate
candidate3 =
  Candidate {candidateId = 3, codeReview = A, cultureFit = B, education = MS}

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1, candidate1), (2, candidate2), (3, candidate3)]

--
-- The Maybe context
--
assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

--
-- The List context
--
candidates :: [Candidate]
candidates = [candidate1, candidate2, candidate3]

--  Working with lists by using the tools of the Monad type class, you can
--  treat entire lists as single values.
assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement

-- A general function that works for any Monad!
assessCandidate :: Monad m => m Candidate -> m String
assessCandidate m = do
  candidate <- m
  let passed = viable candidate
  let statement =
        if passed
          then "passed"
          else "failed"
  return statement
