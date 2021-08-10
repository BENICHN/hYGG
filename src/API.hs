{-# LANGUAGE OverloadedStrings #-}

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

decoResp :: Action b -> Action b
decoResp = (setHeader "Access-Control-Allow-Origin" "*" >>)

app :: Config -> API
app c = do
  get "ping" $ decoResp $ text "pong"
  get "config" $ decoResp $ json c
  get "search" $ decoResp $ do
    params >>= liftIO . parseSearchTorrents c >>= witherr' json
  get ("torrent" <//> wildcard) $ \s -> decoResp $ liftIO (parseTorrentInfos c $ unpack s) >>= witherr' json
  get ("dl" <//> var) $ \stid -> decoResp $ do
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