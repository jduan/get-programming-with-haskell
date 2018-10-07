module Monad.MapLookups where

import qualified Data.Map as Map

type UserName = String

type GamerId = Int

type PlayerCredits = Int

type WillCoId = Int

userNameDB :: Map.Map GamerId UserName
userNameDB =
  Map.fromList
    [ (1, "nYarlathoTep")
    , (2, "KINGinYELLOW")
    , (3, "dagon1997")
    , (4, "rcarter1919")
    , (5, "xCTHULHUx")
    , (6, "yogSOThoth")
    ]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB =
  Map.fromList
    [ ("nYarlathoTep", 2000)
    , ("KINGinYELLOW", 15000)
    , ("dagon1997", 300)
    , ("rcarter1919", 12)
    , ("xCTHULHUx", 50000)
    , ("yogSOThoth", 150000)
    ]

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB =
  Map.fromList
    [(1001, 1), (1002, 2), (1003, 3), (1004, 4), (1005, 5), (1006, 6)]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

lookupUserName :: GamerId -> Maybe UserName
lookupUserName gid = Map.lookup gid userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId gid = lookupUserName gid >>= lookupCredits

creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits

echo :: IO ()
echo = putStrLn "Enter a string and we'll echo it!" >> getLine >>= putStrLn

readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print (n * 2)

readAndPrintDouble :: IO ()
readAndPrintDouble = readInt >>= printDouble

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName =
  askForName >> getLine >>= (\name -> return $ nameStatement name) >>= putStrLn

add2 :: Num a => a -> IO a
add2 n = return (n + 2)

-- These 2 exercises show that Monad is strictly more powerful than Functor
-- and Applicative.
--
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f m = f <$> m

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mf ma = mf <*> ma

-- Implement bind for Maybe
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just a) f = f a
