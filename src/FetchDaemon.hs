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
  -- repos <- gitRepos path
  let sleepSeconds = fromMaybe defaultSleepSeconds $ fmap realToFrac maybeSecs :: NominalDiffTime
  fetch path sleepSeconds

gitRepos :: FilePath -> IO [FilePath]
gitRepos path = do
  let shellPaths = find (suffix ".git") path :: Shell FilePath
  paths <- fold shellPaths Fold.list
  let filtps = filter is_git paths
  return $ fmap parent filtps

is_git :: FilePath -> Bool
is_git path = do
  let pathString = format fp path
  let git_suffix = T.isSuffixOf ".git" pathString
  let excluded = T.isInfixOf ".stack-work" pathString
  git_suffix && not excluded

fetch :: FilePath -> NominalDiffTime -> IO ()
fetch repoDir sleepSeconds = do
  cd repoDir
  dir <- pwd

  fetchOne dir .||. die (format ("Error fetching in "%fp%". Is this a git repository?") dir)

  log $ format ("Fetching every "%s%" in " %fp% "") (T.pack $ show sleepSeconds) dir
  fetchEvery dir sleepSeconds

fetchOne :: FilePath -> IO ExitCode
fetchOne dir = do
  log $ format ("Running a git fetch in " %fp) dir
  (exitCode, out) <- shellStrict "git fetch" empty
  log $ format ("Done running a git fetch in " %fp% " - "%s%s) dir (T.pack $ show exitCode) out
  return exitCode

fetchEvery :: FilePath -> NominalDiffTime -> IO ()
fetchEvery dir sleepSeconds = do
  sleep sleepSeconds
  fetchOne dir
  fetchEvery dir sleepSeconds

log :: Text -> IO ()
log msg = do
  now <- date
  echoFlush $ format (utc%" "%s) now msg
