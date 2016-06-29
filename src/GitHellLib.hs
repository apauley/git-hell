{-# LANGUAGE OverloadedStrings #-}

module GitHellLib
    ( currentBranch
    ) where

import Turtle

currentBranch :: Shell Text
currentBranch = do
  let branches = inproc "git" ["branch", "--list"] empty
  sed ("* " *> return "") $ grep (prefix "*") branches
