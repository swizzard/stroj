{-# LANGUAGE OverloadedStrings #-}
module Toot
    ( getClient
    , toot
    ) where

import System.Environment (lookupEnv)
import System.Random (randomRIO)
import Text.Printf (printf)
import Util
import Web.Hastodon
import Wordfilter (blacklisted)


getClient :: IO (Maybe HastodonClient)
getClient = do
  h <- lookupEnv "host"
  t <- lookupEnv "token"
  return $ HastodonClient <$> h <*> t

newWord :: String -> IO String
newWord w = do
  nw <- (:) <$> getLetter <*> pure (tail w)
  bl <- blacklisted nw
  if bl then newWord w
        else return nw

makeToot :: String -> IO String
makeToot w = do
  nw <- newWord w
  return $ printf "%s (jean %s)" w nw

getTootText :: IO String
getTootText = getWord >>= makeToot

toot :: HastodonClient -> IO ()
toot c = getTootText >>= postStatus c >> return ()
