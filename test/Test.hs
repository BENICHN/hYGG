{-# LANGUAGE OverloadedStrings #-}

import YGG
import ScrapHTML
import Text.XML.HXT.Core
import Paths_hYGG
import Config
import Utils
import Data.Maybe

c = Config {port=8080, hostName="https://www3.yggtorrent.nz", yggid="", yggpass="", yggcookie=""}

exSearchurl = "https://www3.yggtorrent.nz/engine/search?name=luca&do=search&order=asc&sort=name&page=50"
exTIurl = "https://www3.yggtorrent.nz/torrent/audio/musique/496268-andy+y+lucas+discography+albums+2003+2018+mp3+320+freek911"

testConfig =
     setDefaultCookie c >>= print

testTI =
    runX (getdoc c exTIurl /> hasName "html" /> hasName "head" /> filterA (getName >>> isA (/="script"))) >>= print

testWr =
    runLA $ root [] [mkelem "aah" [] []] >>> writeDocumentToString [withOutputHTML]

testUD = do
    connectCookie c
    r <- runX $ getdoc c (hostName c <> "/user/account") >>> selectUserData >>> xunpickleVal xpTree
    print r

main = testUD