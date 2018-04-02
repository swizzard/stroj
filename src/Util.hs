{-# LANGUAGE OverloadedStrings #-}
module Util (getWord) where

import Data.Traversable (for)
import System.Environment (lookupEnv, setEnv)
import Paths_jot (getDataFileName)

getWords :: IO [String]
getWords = getDataFileName "data/words.txt" >>=
           readFile >>= return . lines

getIx :: IO (Maybe Int)
getIx = (fmap read) <$> lookupEnv "wordIndex"

setIx :: Int -> IO ()
setIx i = setEnv "wordIndex" (show i)

getWord' :: [String] -> Int -> Maybe String
getWord' ws ix | ix >= length ws = Nothing
               | otherwise = Just $ ws !! ix

getWord :: IO (Maybe String)
getWord = do
  ws <- getWords
  ix <- getIx
  let w = ix >>= getWord' ws
  for ((,) <$> w <*> ix) $ \(w,i) -> setIx (i + 1) >> return w
