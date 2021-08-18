{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module FileTree where

import Data.Aeson
import GHC.Generics
import Data.List
import Data.Either.Extra

type FileTree a = [FileNode a]
data FileNode a =
    Directory {
      dirname :: String,
      dircontent :: FileTree a }
  | File {
      filename :: String,
      filedetails :: a }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON a => ToJSON (FileNode a)
instance FromJSON a => FromJSON (FileNode a)

buildFileTree :: FileTree a -> FileTree a
buildFileTree files =                                         -- /!\ must be a flat tree
  let (filesf, filesd) = mapPartition (\f -> 
        let (p, s) = mapName (break (=='/')) f
         in if null s then Left f else Right (p, changeName (const $ tail s) f)) files
      dirs = (\(p, c) -> Directory {dirname=p, dircontent=buildFileTree (snd <$> c)}) <$> groupBy' fst filesd
  in dirs ++ filesf

flattenFileTree :: FileTree a -> FileTree a
flattenFileTree ft =
  let (fs, ds) = partition (\case File _ _ -> True; _ -> False) ft
   in concatMap (\Directory {dirname=dn, dircontent=dc} -> flattenFileTree $ changeName ((dn <> "/") <>) <$> dc) ds <> fs

changeName :: (String -> String) -> FileNode d -> FileNode d
changeName f = \case
  File {filename=fn, filedetails=fd} -> File {filename=f fn, filedetails=fd}
  Directory {dirname=dn, dircontent=dc} -> Directory {dirname=f dn, dircontent=dc}

mapName :: (String -> b) -> FileNode d -> b
mapName f = \case
  File {filename=fn, filedetails=_} -> f fn
  Directory {dirname=dn, dircontent=_} -> f dn

mapPartition :: (a -> Either b c) -> [a] -> ([b], [c])
mapPartition f t =
  let (ls, rs) = partition isLeft $ f <$> t
   in (fromLeft' <$> ls, fromRight' <$> rs)

groupBy' :: Ord a => (t -> a) -> [t] -> [(a, [t])]
groupBy' p = ((\l -> (fst $ head l, snd <$> l)) <$>) . groupBy (\(y1, _) (y2, _) -> y1 == y2) . sortBy (\(y1, _) (y2, _) -> compare y1 y2) . ((\x -> (p x, x)) <$>)