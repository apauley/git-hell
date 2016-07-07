{-# LANGUAGE OverloadedStrings #-}

module GitHellLib where

import Turtle
import qualified Control.Foldl as Fold
import HSHLib (emptyErrorText)

currentBranchOrEmptyText :: IO Text
currentBranchOrEmptyText = do
  maybeBranch <- currentBranchOrNothing
  let branch = case maybeBranch of
        Just b  -> b
        Nothing -> ""
  return branch

currentBranchOrNothing :: IO (Maybe Text)
currentBranchOrNothing = fold currentBranch Fold.head

currentBranch :: Shell Text
currentBranch = do
  let branches = inprocWithErr "git" ["branch", "--list"] empty
  sed ("* " *> return "") $ grep (prefix "*") (fmap emptyErrorText branches)
