module PizzaCalculator where

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
