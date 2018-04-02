module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Toot

sleepFor :: Int
sleepFor = 60 * 60 * 1000000

main :: IO ()
main = do
  client <- getClient
  case client of
    Nothing -> print "configuration error"
    (Just c) -> forever $ toot c >> threadDelay sleepFor
