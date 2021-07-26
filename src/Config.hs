{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Config where

import GHC.Generics
import Data.Aeson
import Network.Wreq
import Control.Lens
import Network.HTTP.Types.Header
import Utils

data Config = Config {
  port :: Int,
  hostName :: String,
  yggid :: String,
  yggpass :: String,
  yggcookie :: String }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Config
instance FromJSON Config

getConfig :: FilePath -> IO (Maybe Config)
getConfig fp = do
  decodeFileStrict' fp >>= \case
    Nothing -> return Nothing
    Just c -> if yggcookie c == "" then Just <$> setDefaultCookie c else return $ Just c

setDefaultCookie :: Config -> IO Config
setDefaultCookie c = do
  cs <- decS . (^. responseHeader hSetCookie) <$> get (hostName c)
  let ck = takeWhile (/=';') . tail . dropWhile (/='=') $ cs
  return $ c {yggcookie=ck}