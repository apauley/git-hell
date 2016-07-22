{-# LANGUAGE OverloadedStrings #-}

module FetchDaemon where

import Turtle
import Prelude hiding (FilePath, log)
import Data.Maybe
import qualified Data.Text as T (isSuffixOf, isInfixOf, pack)
import qualified Control.Foldl as Fold
import HSHLib (echoFlush)
import Data.Foldable (for_)

defaultSleepSeconds = 120

fetchDaemon :: FilePath -> Maybe Int -> IO ()
fetchDaemon path maybeSecs = do
  let sleepSeconds = fromMaybe defaultSleepSeconds $ fmap realToFrac maybeSecs :: NominalDiffTime
  repos <- gitRepos path
  fetchAll sleepSeconds repos

fetchAll :: NominalDiffTime -> [FilePath] -> IO ()
fetchAll sleepSeconds repos = do
  log $ format ("Fetching "%d%" repo(s) every "%s%"\n") (length repos) (repr sleepSeconds)
  for_ repos fetchOne
  echoFlush ""
  log $ format ("Sleeping "%s) (repr sleepSeconds)
  echoFlush ""
  sleep sleepSeconds
  fetchAll sleepSeconds repos

fetchOne :: FilePath -> IO ExitCode
fetchOne dir' = do
  dir <- realpath dir'
  log $ format ("Running a git fetch in " %fp) dir
  (exitCode, out) <- shellStrict "git fetch" empty
  log $ format ("Done running a git fetch in " %fp% " - "%s%s) dir (T.pack $ show exitCode) out
  return exitCode

gitRepos :: FilePath -> IO [FilePath]
gitRepos path = do
  let shellPaths = find (suffix ".git") path :: Shell FilePath
  paths <- fold shellPaths Fold.list
  let filtps = filter is_git paths
  let relativeRepos = fmap parent filtps
  return relativeRepos

is_git :: FilePath -> Bool
is_git path = do
  let pathString = format fp path
  let git_suffix = T.isSuffixOf ".git" pathString
  let excluded = T.isInfixOf ".stack-work" pathString
  git_suffix && not excluded

log :: Text -> IO ()
log msg = do
  now <- date
  echoFlush $ format (utc%" "%s) now msg
