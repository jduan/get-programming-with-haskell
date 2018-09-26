{-# LANGUAGE OverloadedStrings #-}

module TextExample where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

helloPerson :: T.Text -> T.Text
helloPerson name = "Hello" <> " " <> name <> "!"

mainText :: IO ()
mainText = do
  TIO.putStrLn "Hello! What's your name?"
  name <- TIO.getLine
  let statement = helloPerson name
  TIO.putStrLn statement
