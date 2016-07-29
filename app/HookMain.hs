{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import System.Environment (getProgName)
import qualified Data.Text as T (pack, strip, words)

main :: IO ()
main = do
  me' <- getProgName
  let me = T.pack me'
  defaultMain me
  case me of
    "pre-push" -> do
      (remote, url) <- options "git pre-push hook" prePushParser
      prePushMain remote url
    "pre-receive" -> preReceiveMain
    name -> echo $ format (s%" support not implemented") name

defaultMain :: Text -> IO ()
defaultMain me = do
  echo $ format ("Script name: >"%s%"<") me
  args <- arguments
  echo $ format ("Args: >"%s%"<\n") $ repr args
  dir <- pwd
  echo $ format ("pwd: >"%fp%"<\n") dir

prePushMain :: Text -> Text -> IO ()
prePushMain remote url = do
  echo $ format ("Remote: "%s%" Url: "%s) remote url
  input <- strict stdin
  echo $ (format ("\n stdin: >"%s%"<") input)
  let [localRef, localSha, remoteRef, remoteSha] = (T.words . T.strip) input
  echo $ format ("Local  Ref: "%s%" Local  SHA: "%s) localRef localSha
  echo $ format ("Remote Ref: "%s%" Remote SHA: "%s) remoteRef remoteSha

  echo "\n Local commit: >>>"
  stdout $ catCommit localSha
  echo "<<< local"

  echo "\n Remote commit: >>>"
  stdout $ catCommit remoteSha
  echo "<<< remote"

preReceiveMain :: IO ()
preReceiveMain = do
  evars <- env
  echo $ format ("Env: \n>"%s%"<\n") $ repr evars
  input <- strict stdin
  echo $ (format ("\n stdin: >"%s%"<\n") input)
  let [oldSha, newSha, refName] = (T.words . T.strip) input
  echo $ format ("Old: "%s%" New: "%s%" Ref: "%s) oldSha newSha refName

  echo "\n Old: >>>"
  stdout $ catCommit oldSha
  echo "<<< old"

  echo "\n New: >>>"
  stdout $ catCommit newSha
  echo "<<< new\n"

catCommit :: Text -> Shell Text
catCommit commit = do
  inproc "git" ["cat-file", "commit", commit] empty

prePushParser :: Parser (Text, Text)
prePushParser = (,) <$> argText "remote"  "Name of the remote to which the push is being done"
                    <*> argText "url"     "URL to which the push is being done"
