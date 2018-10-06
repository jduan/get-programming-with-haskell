module Monad.MonadExercises where

{-

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

-}
-- EXERCISE 1: Implement `Functor`, `Applicative`,
-- and `Monad` instances for the `Maybe` monad:
data Maybe2 a
  = Just2 a
  | Nothing2
  deriving (Show) -- Useful for debugging

instance Functor Maybe2 where
  fmap f Nothing2 = Nothing2
  fmap f (Just2 a) = Just2 (f a)

instance Applicative Maybe2 where
  pure = Just2
  Nothing2 <*> _ = Nothing2
  _ <*> Nothing2 = Nothing2
  Just2 f <*> Just2 a = Just2 (f a)

instance Monad Maybe2 where
  Nothing2 >>= _ = Nothing2
  Just2 a >>= f = f a

-- EXERCISE 2: Use the `Maybe` monad (with `do`
-- notation) to implement a function that adds
-- two `Maybe Int` values.
addMaybe :: Monad m => m Int -> m Int -> m Int
addMaybe m1 m2 = do
  i1 <- m1
  i2 <- m2
  return (i1 + i2)

addMaybe' :: Monad m => m Int -> m Int -> m Int
addMaybe' m1 m2 = m1 >>= (\i1 -> m2 >>= (\i2 -> return (i1 + i2)))

-- EXERCISE 3: If we didn't have the `Monad` instance
-- for `Maybe`, could we do EXERCISE 2 using the
-- `Applicative` instance?
addMaybe2 :: Applicative f => f Int -> f Int -> f Int
addMaybe2 f1 f2 = pure (+) <*> f1 <*> f2

-- EXERCISE 4: Implement `Functor`, `Applicative`,
-- and `Monad` instances for the list monad.
-- Hint: Define the following helper function:
--   concat2 :: List (List a) -> List a
data List a
  = Empty
  | Cons a
         (List a)
  deriving (Show) -- Useful for debugging

instance Functor List where
  fmap f Empty = Empty
  fmap f (Cons a ls) = Cons (f a) (fmap f ls)

instance Applicative List where
  pure a = Cons a Empty
  (<*>) = undefined -- Fill this in.

instance Monad List where
  (>>=) = undefined -- Fill this in.

-- EXERCISE 5: Use the list monad (with `do` notation) to
-- implement a function which takes two lists and returns
-- a list of all pairs of elements chosen from those lists.
-- For example, [1, 2] and [3, 4] would produce
-- [(1, 3), (1, 4), (2, 3), (2, 4)].
allPairs :: List a -> List b -> List (a, b)
allPairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)

-- EXERCISE 6: Do EXERCISE 4 without using `do` notation.
allPairsWithoutDo :: List a -> List b -> List (a, b)
allPairsWithoutDo xs ys = xs >>= \x -> ys >>= \y -> return (x, y)

-- EXERCISE 7: Define the data type for the `State` monad.
-- Your solution goes here.
-- EXERCISE 8: Implement `Functor`, `Applicative`, and
-- `Monad` instances for the `State` monad.
-- Your solution goes here.
-- You can use this to print things for debugging.
main2 = print "Hello, World!"

-- Ask the user for a number, double it, and print the value
doubleIt :: IO ()
doubleIt = do
  line <- getLine
  let num = read line :: Int
  print (2 * num)
