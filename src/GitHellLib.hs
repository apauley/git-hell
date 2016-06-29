{-# LANGUAGE OverloadedStrings #-}

module GitHellLib where

import Turtle
import qualified Control.Foldl as Fold

currentBranch :: Shell Text
currentBranch = do
  let branches = inproc "git" ["branch", "--list"] empty
  sed ("* " *> return "") $ grep (prefix "*") branches

currentBranchOrNothing :: IO (Maybe Text)
currentBranchOrNothing = fold currentBranch Fold.head

currentBranchOrEmptyText :: IO Text
currentBranchOrEmptyText = do
  maybeBranch <- currentBranchOrNothing
  let branch = case maybeBranch of
        Just b  -> b
        Nothing -> ""
  return branch
