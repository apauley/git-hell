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
    _               -> echo "Not implemented"

data Command = CurrentBranch (Maybe Text) | Status (Maybe Text) deriving (Show)

parser :: Parser Command
parser = fmap CurrentBranch (subcommand "current-branch" "Show the current git branch" noArgs)
     <|> fmap Status        (subcommand "st" "git status" noArgs)

noArgs :: Parser (Maybe Text)
noArgs = optional (argText "" "")
