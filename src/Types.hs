{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Text (Text, pack)
import GHC.Generics
import Web.Spock

data SLC = SLC {
  seeders :: Int,
  leechers :: Int,
  compl :: Int }
  deriving (Generic, Eq, Ord, Show)
  
data File = File {
  name :: String,
  size :: String }
  deriving (Generic, Eq, Ord, Show)

data TorrentFile = TorrentFile {
  fileinfo :: File,
  cat :: Int,
  torurl :: String,
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
  content :: [File],
  nfo :: String,
  uploader :: Maybe Uploader,
  date :: String,
  presentation :: String,
  commentaries :: [Commentary] }
  deriving (Generic, Eq, Ord, Show)

data User = User {
  userurl :: String,
  avatarurl :: String,
  username :: String,
  role :: String,
  upsize :: String,
  downsize :: String }
  deriving (Generic, Eq, Ord, Show)

data Commentary = Commentary {
  user :: User,
  comage :: String,
  comcontent :: Text }
  deriving (Generic, Eq, Ord, Show)

instance ToJSON SLC
instance FromJSON SLC
instance ToJSON Uploader
instance FromJSON Uploader
instance ToJSON User
instance FromJSON User
instance ToJSON Commentary
instance FromJSON Commentary
instance ToJSON File
instance FromJSON File
instance ToJSON TorrentFile
instance FromJSON TorrentFile
instance ToJSON TorrentInfo
instance FromJSON TorrentInfo

type API = SpockM () () () ()

type APIAction a = SpockAction () () () a