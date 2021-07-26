module Main where

import API
import Utils

main :: IO ()
main = do
  fp <- getExecutableDir
  runAPI (fp <> "config.json")

ghcimain :: IO ()
ghcimain = do
  runAPI "res/config.json"