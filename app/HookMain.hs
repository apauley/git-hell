{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import System.Environment (getProgName)

main :: IO ()
main = do
  me <- getProgName
  case me of
    "pre-push" -> do
      (remote, url) <- options "git pre-push hook" prePushParser
      echo $ format ("Remote: "%s%" Url: "%s) remote url
    name -> putStrLn name

prePushParser :: Parser (Text, Text)
prePushParser = (,) <$> argText "remote"  "Name of the remote to which the push is being done"
                    <*> argText "url"     "URL to which the push is being done"
