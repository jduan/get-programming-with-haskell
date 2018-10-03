module Functor.FunctorExercises where

import qualified Data.Map as Map

successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe Nothing = Nothing
incMaybe (Just i) = Just (i + 1)

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing = Nothing
reverseMaybe (Just s) = Just (reverse s)

successStr :: Maybe String
successStr = show <$> successfulRequest

failedStr :: Maybe String
failedStr = show <$> failedRequest

data RobotPart = RobotPart
  { name :: String
  , description :: String
  , cost :: Double
  , count :: Int
  } deriving (Show, Eq)

leftArm :: RobotPart
leftArm =
  RobotPart
    { name = "left arm"
    , description = "left arm for face punching!"
    , cost = 1000.00
    , count = 3
    }

rightArm :: RobotPart
rightArm =
  RobotPart
    { name = "right arm"
    , description = "right arm for kind hand gestures"
    , cost = 1025.00
    , count = 5
    }

robotHead :: RobotPart
robotHead =
  RobotPart
    { name = "robot head"
    , description = "this head looks mad"
    , cost = 5092.25
    , count = 2
    }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part =
  mconcat
    [ "<h2>"
    , partName
    , "</h2>"
    , "<p><h3>desc</h3>"
    , partDesc
    , "</p>"
    , "<p><h3>cost</h3>"
    , partCost
    , "</p>"
    , "<p><h3>count</h3>"
    , partCount
    , "</p>"
    ]
  where
    partName = name part
    partDesc = description part
    partCost = (show . cost) part
    partCount = (show . count) part

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

insertSnippet :: Maybe Html -> IO ()
insertSnippet = undefined

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

-- Find cost of a part by ID
partCost :: Int -> Maybe Double
partCost i = cost <$> Map.lookup i partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDB

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

-- For IO, it's essential to be able to change values (fmap over it) in an
-- IO context, because you can't take IO values out of their context.
htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

newtype Box a =
  Box a
  deriving (Show, Eq)

instance Functor Box where
  fmap f (Box a) = Box (f a)

morePresents :: Int -> Box a -> Box [a]
morePresents n (Box a) = Box $ replicate n a
