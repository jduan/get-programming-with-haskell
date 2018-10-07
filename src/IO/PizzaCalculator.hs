module IO.PizzaCalculator where

import qualified Data.Map as Map

area :: Double -> Double
area diameter = pi * (diameter / 2) ^ 2

type Pizza = (Double, Double)

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / area size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 =
  if costPerInch p1 < costPerInch p2
    then p1
    else p2

describePizza :: Pizza -> String
describePizza (size, cost) =
  "The " ++ show size ++ " pizza is cheaper in terms of cost per square inch"

mainPizza :: IO ()
mainPizza = do
  putStrLn "What is the size of pizza 1"
  size1 <- getLine
  putStrLn "What is the cost of pizza 1"
  cost1 <- getLine
  putStrLn "What is the size of pizza 2"
  size2 <- getLine
  putStrLn "What is the cost of pizza 2"
  cost2 <- getLine
  let pizza1 = (read size1, read cost1)
  let pizza2 = (read size2, read cost2)
  let cheaperPizza = comparePizzas pizza1 pizza2
  putStrLn (describePizza cheaperPizza)

-- do-notation desugared
mainPizza' :: IO ()
mainPizza' =
  putStrLn "What is the size of pizza 1" >> getLine >>= \size1 ->
    putStrLn "What is the cost of pizza 1" >> getLine >>= \cost1 ->
      putStrLn "What is the size of pizza 2" >> getLine >>= \size2 ->
        putStrLn "What is the cost of pizza 2" >> getLine >>= \cost2 ->
          let pizza1 = (read size1, read cost1)
              pizza2 = (read size2, read cost2)
              cheaperPizza = comparePizzas pizza1 pizza2
           in putStrLn (describePizza cheaperPizza)

costData :: Map.Map Int Double
costData = Map.fromList [(1, 18.0), (2, 16.0)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 20.0), (2, 15.0)]

-- This looks almost the same as the mainPizza function!
maybeMain :: Maybe String
maybeMain = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 costData
  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 costData
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let cheaperPizza = comparePizzas pizza1 pizza2
  return (describePizza cheaperPizza)

-- Rewrite the function above so it works with the List type.
-- Don't worry if the results seem strange.
maybeMain' :: [String]
maybeMain' = do
  size1 <- [10, 15]
  cost1 <- [25, 40]
  size2 <- [12, 20]
  cost2 <- [20, 38]
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let cheaperPizza = comparePizzas pizza1 pizza2
  return (describePizza cheaperPizza)

-- Refactor the maybeMain function so that it works with any Monad.
maybeMainM ::
     Monad m => m Double -> m Double -> m Double -> m Double -> m String
maybeMainM s1 c1 s2 c2 = do
  size1 <- s1
  cost1 <- c1
  size2 <- s2
  cost2 <- c2
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let cheaperPizza = comparePizzas pizza1 pizza2
  return (describePizza cheaperPizza)
