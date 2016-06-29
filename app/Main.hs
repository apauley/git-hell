{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)

main :: IO ()
main = do
  view currentBranch
  echo "Hello git-hell"

currentBranch :: Shell Text
currentBranch = do
  let branches = inproc "git" ["branch", "--list"] empty
  sed ("* " *> return "") $ grep (prefix "*") branches
