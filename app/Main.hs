{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import GitHellLib
import Prelude hiding (FilePath)

main :: IO ()
main = do
  x <- options "git utilities" parser
  case x of
    CurrentBranch _ -> stdout currentBranch
    FetchDaemon (path, sleepSeconds) -> echo "Not implemented"

data Command = CurrentBranch (Maybe Text) | FetchDaemon (FilePath, Maybe Int) deriving (Show)

parser :: Parser Command
parser = fmap CurrentBranch (subcommand "current-branch" "Show the current git branch" noArgs)
     <|> fmap FetchDaemon   (subcommand "fetch-daemon" "git status" fetchParser)

fetchParser :: Parser (FilePath, Maybe Int)
fetchParser = (,) <$> argPath "repo"  "The path to a git repository"
                  <*> optional (argInt "sleepSeconds" "The number of seconds to sleep between fetches.")

noArgs :: Parser (Maybe Text)
noArgs = optional (argText "" "")
