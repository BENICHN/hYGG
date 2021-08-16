{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module API where

import Control.Monad.IO.Class
import YGG
import Types
import Utils
import Web.Spock hiding (head)
import Data.Maybe
import Data.Text (pack, unpack)
import Network.Wai
import Config
import Web.Spock.Config

app :: Config -> API
app c = do
  -- hookRoute OPTIONS wildcard \_ -> do
  --   setHeader "Access-Control-Allow-Origin" "*"
  --   setHeader "Access-Control-Allow-Headers" "*"
  --   setHeader "Allow" "GET"
  --   text ""
  get "ping" $ text "pong"
  get "config" $ json c
  get "user" $ liftIO (do
    connectCookie c
    liftIO (getUserData c)) >>= witherr' json
  get "search" do
    params >>= liftIO . parseSearchTorrents c >>= witherr' json
  get ("torrent" <//> wildcard) $ \s -> liftIO (parseTorrentInfos c $ unpack s) >>= witherr' json
  get ("dl" <//> var) $ \stid -> do
    lbs <- liftIO $ do
      connectCookie c
      dltorrent c (read stid)
    setHeader "Content-Type" "application/x-bittorrent"
    name <- param "name"
    setHeader "Content-Disposition" ("inline; filename=\"" <> fromMaybe (pack stid) name <> ".torrent\"")
    response (\s h -> responseLBS s h lbs)

runAPI :: FilePath -> IO ()
runAPI fp = do
  c <- fromJust <$> getConfig fp
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock (port c) $ spock spockCfg $ app c