module Monad.DoNotation where

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
