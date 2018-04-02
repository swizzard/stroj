module Toot
    ( toot
    ) where

import System.Environment (lookupEnv)
import System.Random (randomRIO)
import Text.Printf (printf)
import Web.Hastodon
import Wordfilter (blacklisted)


getClient :: IO (Maybe HastodonClient)
getClient = do
  h <- lookupEnv "host"
  t <- lookupEnv "token"
  return $ HastodonClient <$> h <*> t

letters :: [Char]
letters = "bcdfghjklmnprstvwxyz"

oneLetter :: IO Char
oneLetter = (!!) <$> pure letters <*> (randomRIO (0, length letters - 1))

newWord :: String -> IO String
newWord w = do
  nw <- (:) <$> oneLetter <*> pure (tail w)
  bl <- blacklisted nw
  if bl then newWord w
        else return nw

makeToot :: String -> IO String
makeToot w = do
  nw <- newWord w
  return $ printf "%s (jean %s)" w nw

toot' :: HastodonClient -> String -> IO ()
toot' c w = do
  tootTxt <- makeToot w
  s <- postStatus c tootTxt
  case s of
    (Left err) -> print err
    (Right status) -> print status
  return ()

toot :: String -> IO ()
toot w = do
  c <- getClient
  case c of
    Nothing -> print "configuration error"
    (Just client) -> toot' client w
