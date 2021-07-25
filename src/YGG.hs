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

yggcookieheader :: String -> Options -> Options
yggcookieheader ck = header "Cookie" .~ ["ygg_=" <> encS ck]

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
  res <- (^. responseBody) <$> get (makeTorrentFilesUri c tid)
  return $ fromMaybe "" do
      v <- JS.decode res
      parse' (v .: "html")

parseTorrentInfos :: Config -> String -> IO TorrentInfo
parseTorrentInfos c s =
  let tid = read . takeWhile (/='-') . takeWhileEnd (/='/') $ s
      url = hostName c <> "/torrent/" <> s
  in do
    files <- getFiles c tid
    nfo <- EL.decodeUtf8 . (^. responseBody) <$> get (hostName c <> "/engine/get_nfo?torrent=" <> show tid)
    head <$> runX (getdoc url >>> selectTI (arr (const files) >>> xread >>> removeAllWhiteSpace) >>> xunpickleVal (xpTI (toStrict nfo) url tid))

parseSearchTorrents :: Config -> Int -> String -> IO [TorrentFile]
parseSearchTorrents c p q = runX $ getdoc (makeSearchURI c p q) >>> selectResultsTable >>> xunpickleVal xpTF

makeSearchURI :: Config -> Int -> String -> String
makeSearchURI c p q = hostName c <> "/engine/search?name=" <> encode q <> "&do=search" <> "&page=" <> show p

makeTorrentFilesUri :: Config -> Int -> String
makeTorrentFilesUri c tid = hostName c <> "/engine/get_files?torrent=" <> show tid