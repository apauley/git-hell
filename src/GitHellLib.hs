{-# LANGUAGE OverloadedStrings #-}

module GitHellLib where

import Turtle
import qualified Control.Foldl as Fold
import qualified Data.Text as T (null, strip)
import HSHLib (emptyErrorText)

currentBranchOrNothing :: IO (Maybe Text)
currentBranchOrNothing = do
  br <- currentBranchDiscardErr
  if T.null br then return Nothing else return $ Just br

currentBranchDiscardErr :: IO Text
currentBranchDiscardErr = do
  let shellTxt = sed ("* " *> return "") $ grep (prefix "*") (gitDiscardErr "branch" ["--list"])
  fmap T.strip $ strict shellTxt

gitDiscardErr :: Text -> [Text] -> Shell Text
gitDiscardErr cmd args = do
  let out = inprocWithErr "git" (cmd:args) empty
  fmap emptyErrorText out
