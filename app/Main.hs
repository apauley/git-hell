{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import FetchDaemon (fetchDaemon)
import HSHLib (noArgs)
import GitHellLib

main :: IO ()
main = do
  x <- options "git utilities" parser
  case x of
    CurrentBranch _ -> stdout currentBranchDiscardErr
    FetchDaemon (path, maybeSecs) -> fetchDaemon path maybeSecs

data Command = CurrentBranch (Maybe Text) | FetchDaemon (FilePath, Maybe Int) deriving (Show)

parser :: Parser Command
parser = fmap CurrentBranch (subcommand "current-branch" "Show the current git branch" noArgs)
     <|> fmap FetchDaemon   (subcommand "fetch-daemon" "Runs `git fetch` continuously for the given repositories" fetchParser)

fetchParser :: Parser (FilePath, Maybe Int)
fetchParser = (,) <$> argPath "basedir"  "The path to a directory with git repositories, or the path to a single repo"
                  <*> optional (argInt "sleepSeconds" "The number of seconds to sleep between fetches.")
