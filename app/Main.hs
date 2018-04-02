module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Toot (toot)
import Util (getWord)


sleepFor :: Int
sleepFor = 60 * 60 * 1000000

main :: IO ()
main = do
  w <- getWord
  case w of
    Nothing -> return ()
    (Just w) -> do
      toot w
      threadDelay sleepFor
      main
