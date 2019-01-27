{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Data.Text 
  ( Text,
    pack
  )
import Data.Text.IO as T
import Lib

pshow :: Show a => a -> Text
pshow = pack . show

getScores :: Profile -> IO ()
getScores profile = do
  scores' <- scores profile
  mapM_ (T.putStrLn . pshow) scores'

main :: IO ()
main = do
  maybeProfile <- profile "76561198022498582"
  case maybeProfile of
    Just profile -> getScores profile
    Nothing -> T.putStrLn "No profile was found"

