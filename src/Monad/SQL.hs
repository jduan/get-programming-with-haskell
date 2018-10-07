module Monad.SQL where

import Control.Applicative (Alternative)
import Control.Monad (guard)

data Name = Name
  { firstName :: String
  , lastName :: String
  }

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data GradeLevel
  = Freshman
  | Sophmore
  | Junior
  | Senior
  deriving (Show, Eq, Ord, Enum)

data Student = Student
  { studentId :: Int
  , gradeLevel :: GradeLevel
  , studentName :: Name
  } deriving (Show)

students :: [Student]
students =
  [ Student 1 Senior (Name "Audre" "Lorde")
  , Student 2 Junior (Name "Leslie" "Silko")
  , Student 3 Freshman (Name "Judith" "Butler")
  , Student 4 Senior (Name "Guy" "Debord")
  , Student 5 Sophmore (Name "Jean" "Baudrillard")
  , Student 6 Junior (Name "Julia" "Kristeva")
  ]

_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do
  val <- vals
  return (prop val)

_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where pred vals = do
  val <- vals
  guard (pred val)
  return val

startsWith :: Char -> String -> Bool
startsWith char string = char == head string

data Teacher = Teacher
  { teacherId :: Int
  , teacherName :: Name
  } deriving (Show)

teachers :: [Teacher]
teachers =
  [ Teacher 100 (Name "Simone" "De Beauvior")
  , Teacher 200 (Name "Susan" "Sontag")
  ]

data Course = Course
  { courseId :: Int
  , courseTitle :: String
  -- this is the id of the teacher
  , teacher :: Int
  } deriving (Show)

courses :: [Course]
courses = [Course 101 "French" 100, Course 201 "English" 200]

-- Join 2 lists, checking to see whether a given property of data in one
-- list is equal to another property in another list.
_join ::
     (Monad m, Alternative m, Eq c)
  => m a
  -> m b
  -> (a -> c)
  -> (b -> c)
  -> m (a, b)
_join xs ys selector1 selector2 = do
  x <- xs
  y <- ys
  guard (selector1 x == selector2 y)
  return (x, y)

_hinq selectQuery joinQuery whereQuery = selectQuery $ whereQuery joinQuery

finalResult :: [Name]
finalResult =
  _hinq
    (_select (teacherName . fst))
    (_join teachers courses teacherId teacher)
    (_where ((== "English") . courseTitle . snd))

teacherFirstName :: [String]
teacherFirstName = _hinq (_select firstName) finalResult (_where (const True))
