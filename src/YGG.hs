{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module YGG where

import ScrapHTML
import Network.Wreq
import Data.ByteString.Lazy (ByteString)
import Control.Lens
import Types
import Data.Text.Lazy.Encoding as EL
import Data.Text.Lazy (toStrict)
import Data.List.Extra
import Utils
import Text.XML.HXT.Core
import qualified Data.Aeson as JS
import Data.Maybe
import Data.Aeson.Types ((.:))
import Network.URI.Encode (encode)
import Config
import Data.Text (Text, unpack, pack)
import Network.HTTP.Types.Header
import qualified Data.ByteString.Lazy.Char8 as C8

yggcookieheader :: String -> Options -> Options
yggcookieheader ck = header hCookie .~ ["ygg_=" <> encS ck]

connectCookie :: Config -> IO (Response ByteString)
connectCookie c =
  let opts = defaults & yggcookieheader (yggcookie c)
  in postWith opts (hostName c <> "/user/login") ["id" := yggid c, "pass" := yggpass c]

dltorrent :: Config -> Int -> IO ByteString
dltorrent c tid =
  let opts = defaults & yggcookieheader (yggcookie c)
  in (^. responseBody) <$> getWith opts (hostName c <> "/engine/download_torrent?id=" <> show tid)

getFiles :: Config -> Int -> IO String
getFiles c tid = do
  res <- (^. responseBody) <$> get (hostName c <> "/engine/get_files?torrent=" <> show tid)
  return $ fromMaybe "" do
      v <- JS.decode res
      parse' (v .: "html")

parseTorrentInfos :: Config -> String -> IO (Maybe TorrentInfo)
parseTorrentInfos c s =
  let tid = read . takeWhile (/='-') . takeWhileEnd (/='/') $ s
      url = hostName c <> "/torrent/" <> s
  in do
    files <- getFiles c tid
    nfo <- C8.unpack . (^. responseBody) <$> get (hostName c <> "/engine/get_nfo?torrent=" <> show tid)
    let filesarr = ac files >>> readFromString [withParseHTML True] >>> removeAllWhiteSpace
    listToMaybe <$> runX (getdoc url >>> selectTI c filesarr >>> xunpickleVal (xpTI c nfo url tid))

parseSearchTorrents :: Config -> [(Text, Text)] -> IO (Maybe SearchResult)
parseSearchTorrents c ps = listToMaybe <$> runX (getdoc (hostName c <> "/engine/search?" <> unpack (makeUriQuery ps)) >>> selectResultsTable >>> xunpickleVal (xpTF c))