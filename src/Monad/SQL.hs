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

--
-- Define a data type so you don't have to provide a where clause.
--
data HINQ m a b
  = HINQ (m a -> m b)
         (m a)
         (m a -> m a)
  | HINQ_ (m a -> m b)
          (m a)

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (const True))

-- Because Haskell uses lazy evaluation, simply defining this query doesn't
-- run it. This means you can pass around expensive computation without
-- worrying about running the queries until you need the result.
query1 :: HINQ [] (Teacher, Course) Name
query1 =
  HINQ
    (_select (teacherName . fst))
    (_join teachers courses teacherId teacher)
    (_where ((== "English") . courseTitle . snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName) teachers

possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 =
  HINQ
    (_select (teacherName . fst))
    (_join possibleTeacher possibleCourse teacherId teacher)
    (_where ((== "French") . courseTitle . snd))

missingCourse :: Maybe Course
missingCourse = Nothing

maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 =
  HINQ
    (_select (teacherName . fst))
    (_join possibleTeacher missingCourse teacherId teacher)
    (_where ((== "French") . courseTitle . snd))

--
-- course enrollment
--
data Enrollment = Enrollment
  { student :: Int
  , course :: Int
  } deriving (Show, Eq)

enrollments :: [Enrollment]
enrollments =
  [ Enrollment 1 101
  , Enrollment 2 101
  , Enrollment 2 201
  , Enrollment 3 101
  , Enrollment 4 201
  , Enrollment 4 101
  , Enrollment 5 101
  , Enrollment 6 201
  ]

studentEnrollmentsQuery =
  HINQ_
    (_select
       (\(student, enrollment) -> (studentName student, course enrollment)))
    (_join students enrollments studentId student)

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQuery

studentCoursesQuery =
  HINQ_
    (_select
       (\((studentName, courseId), kourse) -> (studentName, courseTitle kourse)))
    (_join studentEnrollments courses snd courseId)

studentCourses :: [(Name, String)]
studentCourses = runHINQ studentCoursesQuery

-- Given a course name, find all the students that enrolled in that course.
getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
  where
    courseQuery =
      HINQ
        (_select (fst . fst))
        (_join studentEnrollments courses snd courseId)
        (_where ((== courseName) . courseTitle . snd))
