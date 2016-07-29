{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import System.Environment (getProgName)
import qualified Data.Text as T (pack)

main :: IO ()
main = do
  me' <- getProgName
  let me = T.pack me'
  defaultMain me
  case me of
    "pre-push" -> do
      (remote, url) <- options "git pre-push hook" prePushParser
      prePushMain remote url
    name -> echo $ format (s%" support not implemented") name

defaultMain :: Text -> IO ()
defaultMain me = do
  echo $ format ("Script name: >"%s%"<") me
  args <- arguments
  echo $ format ("Args: >"%s%"<\n") $ repr args
  dir <- pwd
  echo $ format ("pwd: >"%fp%"<\n") dir
  evars <- env
  echo $ format ("Env: \n>"%s%"<\n") $ repr evars

prePushMain :: Text -> Text -> IO ()
prePushMain remote url = do
  echo $ format ("Remote: "%s%" Url: "%s) remote url
  input <- strict stdin
  echo $ (format ("\n stdin: >"%s%"<") input)

prePushParser :: Parser (Text, Text)
prePushParser = (,) <$> argText "remote"  "Name of the remote to which the push is being done"
                    <*> argText "url"     "URL to which the push is being done"
