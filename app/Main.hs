module Main where

import Config
import API
import Web.Spock
import Web.Spock.Config
import Data.Maybe
import Utils

main :: IO ()
main = do
  fp <- getExecutableDir
  c <- fromJust <$> getConfig (fp <> "config.json")
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 $ spock spockCfg $ app c