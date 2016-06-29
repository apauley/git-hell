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
  inproc "git" ["branch", "--list"] empty
