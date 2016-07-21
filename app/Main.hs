{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import GitHellLib
import FetchDaemon (fetchDaemon)
import Prelude hiding (FilePath)

main :: IO ()
main = do
  x <- options "git utilities" parser
  case x of
    CurrentBranch _ -> stdout currentBranchDiscardErr
    FetchDaemon (path, maybeSecs) -> fetchDaemon path maybeSecs

data Command = CurrentBranch (Maybe Text) | FetchDaemon (FilePath, Maybe Int) deriving (Show)

parser :: Parser Command
parser = fmap CurrentBranch (subcommand "current-branch" "Show the current git branch" noArgs)
     <|> fmap FetchDaemon   (subcommand "fetch-daemon" "Runs a `git fetch` continuously for a given repository" fetchParser)

fetchParser :: Parser (FilePath, Maybe Int)
fetchParser = (,) <$> argPath "repo"  "The path to a git repository"
                  <*> optional (argInt "sleepSeconds" "The number of seconds to sleep between fetches.")

noArgs :: Parser (Maybe Text)
noArgs = optional (argText "" "")
