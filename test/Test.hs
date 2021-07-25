import YGG
import ScrapHTML
import Text.XML.HXT.Core
import Paths_hYGG
import Config

exSearchurl = "https://www4.yggtorrent.li/engine/search?name=luca&do=search&order=asc&sort=name&page=50"
exTIurl = "https://www4.yggtorrent.li/torrent/audio/musique/496268-andy+y+lucas+discography+albums+2003+2018+mp3+320+freek911"

testConfig =
    let c = Config {hostName="https://www4.yggtorrent.li", yggid="", yggpass="", yggcookie=""}
    in setDefaultCookie c >>= print

main = testConfig