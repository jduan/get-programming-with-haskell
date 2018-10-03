module TimeSeries.TimeSeries where

import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, isNothing)

file1 :: [(Int, Double)]
file1 =
  [ (1, 200.1)
  , (2, 199.5)
  , (3, 199.4)
  , (4, 198.9)
  , (5, 199.0)
  , (6, 200.2)
  , (9, 200.3)
  , (10, 201.2)
  , (12, 202.9)
  ]

file2 :: [(Int, Double)]
file2 =
  [ (11, 201.6)
  , (12, 201.5)
  , (13, 201.5)
  , (14, 203.5)
  , (15, 204.9)
  , (16, 207.1)
  , (18, 210.5)
  , (20, 208.8)
  ]

file3 :: [(Int, Double)]
file3 =
  [ (10, 201.2)
  , (11, 201.6)
  , (12, 201.5)
  , (13, 201.5)
  , (14, 203.5)
  , (17, 210.5)
  , (24, 215.1)
  , (25, 218.7)
  ]

file4 :: [(Int, Double)]
file4 =
  [ (26, 219.8)
  , (27, 220.5)
  , (28, 223.8)
  , (29, 222.8)
  , (30, 223.8)
  , (31, 221.7)
  , (32, 222.3)
  , (33, 220.8)
  , (34, 219.4)
  , (35, 220.1)
  , (36, 220.6)
  ]

data TS a =
  TS [Int]
     [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
  where
    completeTimes = [minimum times .. maximum times]
    timeValueMap = Map.fromList (zip times values)
    extendedValues = map (\t -> Map.lookup t timeValueMap) completeTimes

fileToTS :: [(Int, a)] -> TS a
fileToTS pairs = createTS times values
  where
    (times, values) = unzip pairs

showPair :: Show a => Int -> Maybe a -> String
showPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showPair time Nothing = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat $ zipWith showPair times values

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

tsToMap :: TS a -> Map.Map Int a
tsToMap (TS times values) = foldr f Map.empty (zip times values)
  where
    f (t, Nothing) m = m
    f (t, Just v) m = Map.insert t v m

-- When combining two time series, we favor the right one when there're
-- duplicated keys!
instance Semigroup (TS a) where
  (TS [] []) <> ts2 = ts2
  ts1 <> (TS [] []) = ts1
  (TS t1 v1) <> (TS t2 v2) =
    createTS (Map.keys combinedMap) (Map.elems combinedMap)
    where
      m1 = tsToMap (TS t1 v1)
      m2 = tsToMap (TS t2 v2)
      combinedMap = Map.union m2 m1

instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)

-- data TS a =
--   TS [Int]
--      [Maybe a]
meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS [] []) = Nothing
meanTS (TS times values) =
  if all isNothing values
    then Nothing
    else Just avg
  where
    justVals = filter isJust values
    cleanVals = map fromJust justVals
    avg = realToFrac (sum cleanVals) / realToFrac (length cleanVals)

type CompareFunc a = a -> a -> a

type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
  where
    newFunc (i1, Nothing) (_, Nothing) = (i1, Nothing)
    newFunc (_, Nothing) (i, v) = (i, v)
    newFunc (i, v) (_, Nothing) = (i, v)
    newFunc (i1, Just v1) (i2, Just v2) =
      if func v1 v2 == v1
        then (i1, Just v1)
        else (i2, Just v2)

compareTS :: Eq a => CompareFunc a -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) =
  if all isNothing values
    then Nothing
    else Just best
  where
    best = foldr (makeTSCompare func) (0, Nothing) (zip times values)

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max
