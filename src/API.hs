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

app :: Config -> API
app c = do
  get "search" $ do
    qm <- param "q"
    pm <- param "p"
    withret (\q -> liftIO (parseSearchTorrents c (fromMaybe 0 pm) q) >>= json) qm
  get ("torrent" <//> wildcard) $ \s -> liftIO (parseTorrentInfos c $ unpack s) >>= json
  get ("dl" <//> var) $ \stid -> do
    lbs <- liftIO $ do
      connectCookie c
      dltorrent c (read stid)
    setHeader "Content-Type" "application/x-bittorrent"
    name <- param "name"
    setHeader "Content-Disposition" ("inline; filename=\"" <> fromMaybe (pack stid) name <> ".torrent\"")
    response (\s h -> responseLBS s h lbs)