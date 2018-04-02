{-# LANGUAGE OverloadedStrings #-}
module Util (getLetter, getWord) where

import Data.Random (randomElement, runRVar, StdRandom(..))
import Data.Traversable (for)
import Paths_jot (getDataFileName)

letters :: [Char]
letters = "bcdfghklmnprstvwxyz"

oneOf :: [a] -> IO a
oneOf xs = runRVar (randomElement xs) StdRandom

getWords :: IO [String]
getWords = getDataFileName "data/words.txt" >>=
           readFile >>= return . lines

getWord :: IO String
getWord = getWords >>= oneOf

getLetter :: IO Char
getLetter = oneOf letters
