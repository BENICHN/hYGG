{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Text (Text, pack)
import GHC.Generics
import Web.Spock

data TorCat =
    Karaoke -- 2147
  | Musique
  | Sample
  | PodcastRadio
  
  | AudioBook -- 2151
  | BD
  | Comics
  | Livre
  | Manga
  | Presse

  | Emulateur -- 2157
  | ROM

  | VGLinux -- 2159
  | VGMacOS
  | VGWindows
  | VGMicrosoft
  | VGNintendo
  | VGSony
  | VGSPhone
  | VGTablette
  | VGAutre

  | MapApp -- 2168
  | MapCarte
  | MapDivers

  | PGLinux -- 2171
  | PGMacOS
  | PGWindows
  | PGSPhone
  | PGTablette
  | Formation
  | PGAutre

  | Animation -- 2178
  | AnimationSerie
  | Concert
  | Documentaire
  | Emission
  | Film
  | Serie
  | Spectacle
  | Sport
  | VideoClip

  | XXFilm -- /!\ 2189
  | XXHentai
  | XXImages
  deriving (Generic, Enum, Eq, Ord, Show)

data SLC = SLC {
  seeders :: Int,
  leechers :: Int,
  compl :: Int }
  deriving (Generic, Eq, Ord, Show)

data FileTree = File FileInfo | Directory {
  dirname :: String,
  dircontent :: [FileTree] }
  deriving (Generic, Eq, Ord, Show)
  
data FileInfo = FileInfo {
  name :: String,
  size :: Integer }
  deriving (Generic, Eq, Ord, Show)

data TorrentFile = TorrentFile {
  fileinfo :: FileInfo,
  cat :: TorCat,
  torurlend :: String,
  tid :: Int,
  coms :: Maybe Int,
  age :: String,
  slc :: SLC }
  deriving (Generic, Eq, Ord, Show)

data Uploader = Uploader {
  upurl :: String,
  upname :: String }
  deriving (Generic, Eq, Ord, Show)

data TorrentInfo = TorrentInfo {
  baseinfo :: TorrentFile,
  hash :: String,
  content :: [FileTree],
  nfo :: String,
  uploader :: Maybe Uploader,
  date :: String,
  hour :: String,
  presentation :: String,
  comments :: [Commentary] }
  deriving (Generic, Eq, Ord, Show)

data User = User {
  userurl :: String,
  avatarurl :: String,
  username :: String,
  role :: String,
  upsize :: Integer,
  downsize :: Integer }
  deriving (Generic, Eq, Ord, Show)

data Commentary = Commentary {
  user :: User,
  comage :: String,
  comcontent :: String }
  deriving (Generic, Eq, Ord, Show)

data SearchResult = SearchResult {
  endOfSearch :: Bool,
  searchResults :: [TorrentFile] }
  deriving (Generic, Eq, Ord, Show)

data UserData = UserData {
  udupsize :: Integer,
  uddownsize :: Integer,
  udratio :: Double,
  udactivity :: Bool }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON TorCat
instance FromJSON TorCat
instance ToJSON SLC
instance FromJSON SLC
instance ToJSON Uploader
instance FromJSON Uploader
instance ToJSON User
instance FromJSON User
instance ToJSON Commentary
instance FromJSON Commentary
instance ToJSON FileInfo
instance FromJSON FileInfo
instance ToJSON FileTree
instance FromJSON FileTree
instance ToJSON TorrentFile
instance FromJSON TorrentFile
instance ToJSON SearchResult
instance FromJSON SearchResult
instance ToJSON TorrentInfo
instance FromJSON TorrentInfo
instance ToJSON UserData
instance FromJSON UserData

type API = SpockM () () () ()

type APIAction a = SpockAction () () () a

type Action = ActionCtxT () (WebStateM () () ())