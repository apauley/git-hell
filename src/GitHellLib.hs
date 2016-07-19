{-# LANGUAGE OverloadedStrings #-}

module GitHellLib where

import Turtle
import qualified Control.Foldl as Fold
import HSHLib (emptyErrorText)

currentBranchOrNothing :: IO (Maybe Text)
currentBranchOrNothing = fold currentBranch Fold.head

currentBranch :: Shell Text
currentBranch = do
  sed ("* " *> return "") $ grep (prefix "*") (gitDiscardErr "branch" ["--list"])

gitDiscardErr :: Text -> [Text] -> Shell Text
gitDiscardErr cmd args = do
  let out = inprocWithErr "git" (cmd:args) empty
  fmap emptyErrorText out
