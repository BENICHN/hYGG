{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Utils where

import Text.XML.HXT.Core
import Data.Maybe
import Web.Spock hiding (head)
import Network.HTTP.Types.Status
import Data.Text.Encoding
import Control.Monad.IO.Class
import Data.Text (pack, unpack, Text)
import Data.ByteString (ByteString)
import Data.Aeson.Types
import Data.List.Extra (dropWhileEnd)
import Data.List
import System.Environment
import Network.URI.Encode
import Control.Arrow
import Data.Ratio
import Control.Exception
import Types (Action)

try' :: IO a -> IO (Either SomeException a)
try' = try

readR :: String -> Ratio Integer
readR s =
  let (i, d) = break (=='.') s
      ii = if null i then 0 else read i
      dd = drop 1 d
      dn = if null dd then 1 else read dd
      m = 10 ^ length dd
   in (ii * m + dn) % m

xps :: Monoid m => PU m -> PU m
xps pu = xpWrap (mconcat, (:[])) $ xpList pu

xpTexts :: PU String
xpTexts = xps xpText

groupBy' :: Ord a => (t -> a) -> [t] -> [[t]]
groupBy' p = groupBy (\o1 o2 -> p o1 == p o2) . sortBy (\o1 o2 -> compare (p o1) (p o2))

ac :: Arrow a => c -> a b c
ac = arr . const

makeUriQuery :: [(Text, Text)] -> Text
makeUriQuery params = mconcat . intersperse "&" $ (\(n, v) -> encodeText n <> "=" <> encodeText v) <$> params

getExecutableDir :: IO FilePath
getExecutableDir = dropWhileEnd (/='/') <$> getExecutablePath

slice :: Int -> Int -> [a] -> [a]
slice s l = take l . drop s

encS :: String -> ByteString
encS = encodeUtf8 . pack

decS :: ByteString -> String
decS = unpack . decodeUtf8

withret :: Monad m => (a -> m ()) -> Maybe a -> m ()
withret = maybe $ return ()

witherr' :: (b -> Action ()) -> Maybe b -> Action ()
witherr' = witherr "Erreur"

witherr :: Text -> (b -> Action ()) -> (Maybe b -> Action ())
witherr err f = \case
    Just x -> f x
    Nothing -> setStatus (Status {statusCode=400, statusMessage=encodeUtf8 err})

xpSeq1 :: PU a -> (a -> PU b) -> PU b
xpSeq1 = xpSeq (const undefined)

ignAttr :: PU a -> PU a
ignAttr = xpFilterAttr zeroArrow

ignCont :: PU a -> PU a
ignCont = xpFilterCont zeroArrow

xpElem' :: String -> PU a -> PU a
xpElem' name pua = xpElem name (ignAttr pua)

xpRS :: (Read b, Show b) => PU b
xpRS = xpWrap (read, show) xpText

xpWrapU :: (a -> b) -> PU a -> PU b
xpWrapU f = xpWrap (f, undefined)

parse' :: Parser b -> Maybe b
parse' p = case parse id p of
  Success x -> Just x
  Error _ -> Nothing

changeChildreni :: (ArrowTree a, Tree t, Num i, Enum i) => (i -> Bool) -> a (t b) (t b)
changeChildreni pred = changeChildren (map snd . filter (pred . fst) . zip [0..])

getChildreni :: (ArrowTree a, Tree t, Num i, Enum i) => (i -> Bool) -> a (t b) (t b)
getChildreni p = changeChildreni p >>> getChildren

xpAttr1Elem :: String -> (String, PU a1) -> PU c -> PU (a1, c)
xpAttr1Elem name (a1, pu1) pu = xpElem name (xpFilterAttr (hasName a1) $ xpPair (xpAttr a1 pu1) pu)

xpAttr2Elem :: String -> (String, PU a1) -> (String, PU a2) -> PU c -> PU (a1, a2, c)
xpAttr2Elem name (a1, pu1) (a2, pu2) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2) $ xpTriple (xpAttr a1 pu1) (xpAttr a2 pu2) pu)

xpAttr3Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> PU c -> PU (a1, a2, a3, c)
xpAttr3Elem name (a1, pu1) (a2, pu2) (a3, pu3) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3) $ xp4Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) pu)

xpAttr4Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> PU c -> PU (a1, a2, a3, a4, c)
xpAttr4Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4) $ xp5Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) pu)

xpAttr5Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> PU c -> PU (a1, a2, a3, a4, a5, c)
xpAttr5Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5) $ xp6Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) pu)

xpAttr6Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> PU c -> PU (a1, a2, a3, a4, a5, a6, c)
xpAttr6Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6) $ xp7Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) pu)

xpAttr7Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, c)
xpAttr7Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7) $ xp8Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) pu)

xpAttr8Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, c)
xpAttr8Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8) $ xp9Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) pu)

xpAttr9Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, c)
xpAttr9Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9) $ xp10Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) pu)

xpAttr10Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, c)
xpAttr10Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10) $ xp11Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) pu)

xpAttr11Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, c)
xpAttr11Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11) $ xp12Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) pu)

xpAttr12Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, c)
xpAttr12Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12) $ xp13Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) pu)

xpAttr13Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, c)
xpAttr13Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13) $ xp14Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) pu)

xpAttr14Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, c)
xpAttr14Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14) $ xp15Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) pu)

xpAttr15Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, c)
xpAttr15Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15) $ xp16Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) pu)

xpAttr16Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, c)
xpAttr16Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16) $ xp17Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16) pu)

xpAttr17Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> (String, PU a17) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, c)
xpAttr17Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) (a17, pu17) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16 <+> hasName a17) $ xp18Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16) (xpAttr a17 pu17) pu)

xpAttr18Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> (String, PU a17) -> (String, PU a18) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, c)
xpAttr18Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) (a17, pu17) (a18, pu18) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16 <+> hasName a17 <+> hasName a18) $ xp19Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16) (xpAttr a17 pu17) (xpAttr a18 pu18) pu)

xpAttr19Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> (String, PU a17) -> (String, PU a18) -> (String, PU a19) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, c)
xpAttr19Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) (a17, pu17) (a18, pu18) (a19, pu19) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16 <+> hasName a17 <+> hasName a18 <+> hasName a19) $ xp20Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16) (xpAttr a17 pu17) (xpAttr a18 pu18) (xpAttr a19 pu19) pu)

xpAttr20Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> (String, PU a17) -> (String, PU a18) -> (String, PU a19) -> (String, PU a20) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, c)
xpAttr20Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) (a17, pu17) (a18, pu18) (a19, pu19) (a20, pu20) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16 <+> hasName a17 <+> hasName a18 <+> hasName a19 <+> hasName a20) $ xp21Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16) (xpAttr a17 pu17) (xpAttr a18 pu18) (xpAttr a19 pu19) (xpAttr a20 pu20) pu)

xpAttr21Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> (String, PU a17) -> (String, PU a18) -> (String, PU a19) -> (String, PU a20) -> (String, PU a21) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, c)
xpAttr21Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) (a17, pu17) (a18, pu18) (a19, pu19) (a20, pu20) (a21, pu21) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16 <+> hasName a17 <+> hasName a18 <+> hasName a19 <+> hasName a20 <+> hasName a21) $ xp22Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16) (xpAttr a17 pu17) (xpAttr a18 pu18) (xpAttr a19 pu19) (xpAttr a20 pu20) (xpAttr a21 pu21) pu)

xpAttr22Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> (String, PU a17) -> (String, PU a18) -> (String, PU a19) -> (String, PU a20) -> (String, PU a21) -> (String, PU a22) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, c)
xpAttr22Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) (a17, pu17) (a18, pu18) (a19, pu19) (a20, pu20) (a21, pu21) (a22, pu22) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16 <+> hasName a17 <+> hasName a18 <+> hasName a19 <+> hasName a20 <+> hasName a21 <+> hasName a22) $ xp23Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16) (xpAttr a17 pu17) (xpAttr a18 pu18) (xpAttr a19 pu19) (xpAttr a20 pu20) (xpAttr a21 pu21) (xpAttr a22 pu22) pu)

xpAttr23Elem :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> (String, PU a17) -> (String, PU a18) -> (String, PU a19) -> (String, PU a20) -> (String, PU a21) -> (String, PU a22) -> (String, PU a23) -> PU c -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, c)
xpAttr23Elem name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) (a17, pu17) (a18, pu18) (a19, pu19) (a20, pu20) (a21, pu21) (a22, pu22) (a23, pu23) pu = xpElem name (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16 <+> hasName a17 <+> hasName a18 <+> hasName a19 <+> hasName a20 <+> hasName a21 <+> hasName a22 <+> hasName a23) $ xp24Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16) (xpAttr a17 pu17) (xpAttr a18 pu18) (xpAttr a19 pu19) (xpAttr a20 pu20) (xpAttr a21 pu21) (xpAttr a22 pu22) (xpAttr a23 pu23) pu)

xpAttr1 :: String -> (String, PU a1) -> PU a1
xpAttr1 name (a1, pu1) = xpElem name $ ignCont (xpFilterAttr (hasName a1) $ xpAttr a1 pu1)

xpAttr2 :: String -> (String, PU a1) -> (String, PU a2) -> PU (a1, a2)
xpAttr2 name (a1, pu1) (a2, pu2) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2) $ xpPair (xpAttr a1 pu1) (xpAttr a2 pu2))

xpAttr3 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> PU (a1, a2, a3)
xpAttr3 name (a1, pu1) (a2, pu2) (a3, pu3) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3) $ xpTriple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3))

xpAttr4 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> PU (a1, a2, a3, a4)
xpAttr4 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4) $ xp4Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4))

xpAttr5 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> PU (a1, a2, a3, a4, a5)
xpAttr5 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5) $ xp5Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5))

xpAttr6 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> PU (a1, a2, a3, a4, a5, a6)
xpAttr6 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6) $ xp6Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6))

xpAttr7 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> PU (a1, a2, a3, a4, a5, a6, a7)
xpAttr7 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7) $ xp7Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7))

xpAttr8 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> PU (a1, a2, a3, a4, a5, a6, a7, a8)
xpAttr8 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8) $ xp8Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8))

xpAttr9 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9)
xpAttr9 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9) $ xp9Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9))

xpAttr10 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
xpAttr10 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10) $ xp10Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10))

xpAttr11 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
xpAttr11 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11) $ xp11Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11))

xpAttr12 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
xpAttr12 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12) $ xp12Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12))

xpAttr13 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
xpAttr13 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13) $ xp13Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13))

xpAttr14 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
xpAttr14 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14) $ xp14Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14))

xpAttr15 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
xpAttr15 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15) $ xp15Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15))

xpAttr16 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
xpAttr16 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16) $ xp16Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16))

xpAttr17 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> (String, PU a17) -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
xpAttr17 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) (a17, pu17) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16 <+> hasName a17) $ xp17Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16) (xpAttr a17 pu17))

xpAttr18 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> (String, PU a17) -> (String, PU a18) -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
xpAttr18 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) (a17, pu17) (a18, pu18) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16 <+> hasName a17 <+> hasName a18) $ xp18Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16) (xpAttr a17 pu17) (xpAttr a18 pu18))

xpAttr19 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> (String, PU a17) -> (String, PU a18) -> (String, PU a19) -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
xpAttr19 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) (a17, pu17) (a18, pu18) (a19, pu19) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16 <+> hasName a17 <+> hasName a18 <+> hasName a19) $ xp19Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16) (xpAttr a17 pu17) (xpAttr a18 pu18) (xpAttr a19 pu19))

xpAttr20 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> (String, PU a17) -> (String, PU a18) -> (String, PU a19) -> (String, PU a20) -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
xpAttr20 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) (a17, pu17) (a18, pu18) (a19, pu19) (a20, pu20) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16 <+> hasName a17 <+> hasName a18 <+> hasName a19 <+> hasName a20) $ xp20Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16) (xpAttr a17 pu17) (xpAttr a18 pu18) (xpAttr a19 pu19) (xpAttr a20 pu20))

xpAttr21 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> (String, PU a17) -> (String, PU a18) -> (String, PU a19) -> (String, PU a20) -> (String, PU a21) -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)
xpAttr21 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) (a17, pu17) (a18, pu18) (a19, pu19) (a20, pu20) (a21, pu21) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16 <+> hasName a17 <+> hasName a18 <+> hasName a19 <+> hasName a20 <+> hasName a21) $ xp21Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16) (xpAttr a17 pu17) (xpAttr a18 pu18) (xpAttr a19 pu19) (xpAttr a20 pu20) (xpAttr a21 pu21))

xpAttr22 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> (String, PU a17) -> (String, PU a18) -> (String, PU a19) -> (String, PU a20) -> (String, PU a21) -> (String, PU a22) -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22)
xpAttr22 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) (a17, pu17) (a18, pu18) (a19, pu19) (a20, pu20) (a21, pu21) (a22, pu22) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16 <+> hasName a17 <+> hasName a18 <+> hasName a19 <+> hasName a20 <+> hasName a21 <+> hasName a22) $ xp22Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16) (xpAttr a17 pu17) (xpAttr a18 pu18) (xpAttr a19 pu19) (xpAttr a20 pu20) (xpAttr a21 pu21) (xpAttr a22 pu22))

xpAttr23 :: String -> (String, PU a1) -> (String, PU a2) -> (String, PU a3) -> (String, PU a4) -> (String, PU a5) -> (String, PU a6) -> (String, PU a7) -> (String, PU a8) -> (String, PU a9) -> (String, PU a10) -> (String, PU a11) -> (String, PU a12) -> (String, PU a13) -> (String, PU a14) -> (String, PU a15) -> (String, PU a16) -> (String, PU a17) -> (String, PU a18) -> (String, PU a19) -> (String, PU a20) -> (String, PU a21) -> (String, PU a22) -> (String, PU a23) -> PU (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23)
xpAttr23 name (a1, pu1) (a2, pu2) (a3, pu3) (a4, pu4) (a5, pu5) (a6, pu6) (a7, pu7) (a8, pu8) (a9, pu9) (a10, pu10) (a11, pu11) (a12, pu12) (a13, pu13) (a14, pu14) (a15, pu15) (a16, pu16) (a17, pu17) (a18, pu18) (a19, pu19) (a20, pu20) (a21, pu21) (a22, pu22) (a23, pu23) = xpElem name $ ignCont (xpFilterAttr (hasName a1 <+> hasName a2 <+> hasName a3 <+> hasName a4 <+> hasName a5 <+> hasName a6 <+> hasName a7 <+> hasName a8 <+> hasName a9 <+> hasName a10 <+> hasName a11 <+> hasName a12 <+> hasName a13 <+> hasName a14 <+> hasName a15 <+> hasName a16 <+> hasName a17 <+> hasName a18 <+> hasName a19 <+> hasName a20 <+> hasName a21 <+> hasName a22 <+> hasName a23) $ xp23Tuple (xpAttr a1 pu1) (xpAttr a2 pu2) (xpAttr a3 pu3) (xpAttr a4 pu4) (xpAttr a5 pu5) (xpAttr a6 pu6) (xpAttr a7 pu7) (xpAttr a8 pu8) (xpAttr a9 pu9) (xpAttr a10 pu10) (xpAttr a11 pu11) (xpAttr a12 pu12) (xpAttr a13 pu13) (xpAttr a14 pu14) (xpAttr a15 pu15) (xpAttr a16 pu16) (xpAttr a17 pu17) (xpAttr a18 pu18) (xpAttr a19 pu19) (xpAttr a20 pu20) (xpAttr a21 pu21) (xpAttr a22 pu22) (xpAttr a23 pu23))

diag3 :: c -> (c, c, c)
diag3 x = (x, x, x)