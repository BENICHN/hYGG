{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module ScrapHTML where

import Data.Bits (Bits (xor))
import Data.Char (chr)
import Data.List.Extra (dropEnd, takeEnd, takeWhileEnd, trim)
import Data.List.Split (chunksOf)
import Data.Text (Text, pack)
import Network.URI.Encode (decode)
import Numeric (readHex)
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Types
import Utils

getdoc :: String -> IOStateArrow () XmlTree XmlTree
getdoc url = readDocument [withCurl [], withParseHTML True] url >>> removeAllWhiteSpace >>> decodeMails

{-================================= Search =================================-}

selectResultsTable :: ArrowXml a => a XmlTree XmlTree
selectResultsTable = deep $
      hasName "table"
  >>> hasAttrValue "class" (== "table")
  >>> getChildreni (==1)
  >>> getChildren

xpTF :: PU TorrentFile
xpTF =
    xpElem "tr" $
      xpWrapU (\(cat, (url, name), tid, coms, age, size, compl, seeders, leechers) ->
        TorrentFile {fileinfo=File {name=trim name, size=size}, cat=cat, torurl=url, tid=tid, coms=Just coms, age=trim age, slc=SLC {compl=compl, seeders=seeders, leechers=leechers}}) $
        xp9Tuple
          (xpElem "td" $ xpFilterCont (hasName "div") $ xpElem' "div" xpRS) -- Section
          (xpElem' "td" $ xpAttr1Elem "a" ("href", xpText) xpText) -- URL & nom
          (xpElem "td" $ xpAttr1 "a" ("target", xpRS)) -- NFO
          (xpElem "td" $ xpFilterCont isText xpRS) -- Comments
          (xpElem "td" $ xpFilterCont isText xpText) -- Age
          (xpElem "td" xpText) -- Taille
          (xpElem "td" xpRS) -- Compl
          (xpElem "td" xpRS) -- Seed
          (xpElem "td" xpRS) -- Leech

{-================================= TorrentInfos =================================-}

selectTI :: ArrowXml a => a XmlTree XmlTree -> a XmlTree XmlTree
selectTI files =
  mkelem
    "div"
    []
    [
      deep (
        hasName "main" >>> getChildren >>> getChildreni (== 1)
          >>> ( (getChildreni (== 2) >>> getChildreni (== 1) >>> getChildren >>> getChildren >>> changeChildren (takeEnd 2 . dropEnd 1)
                  >>> ( (getChildreni (== 0) >>> getChildreni odd >>> getChildren)
                          <+> (getChildreni (== 1) >>> getChildreni (== 1) >>> getChildreni (== 1) >>> getChildren >>> getChildren >>> getChildren >>> getChildreni (/= 1) >>> getChildreni (== 1))
                      ))
                  <+> (getChildreni (== 3) >>> changeChildreni (== 2))
                  <+> (getChildreni (== 5) >>> getChildren >>> getChildreni (== 1))
              ))
          <+> (files >>> getChildren)
    ]

xpTI :: Text -> String -> Int -> PU TorrentInfo
xpTI nfo url tid =
  xpWrapU (\(slc, name, cat, size, hash, uploader, (date, age), presentation, coms, files) ->
    TorrentInfo {baseinfo=TorrentFile {fileinfo=File {name=name, size=size}, cat=cat, torurl=url, tid=tid, coms=Nothing, age=age, slc=slc}, hash=hash, content=files, nfo=nfo, uploader=uploader, date=date, presentation=pack presentation, commentaries=coms}) $
    xpElem "div" $ xp10Tuple
      xpSLC
      xpName
      xpCat
      xpSize
      xpHash
      xpUploader
      xpDate
      xpPres
      xpComs
      xpFiles
  where
    xpSoLoC = xpElem' "strong" $ xpWrapU (read . filter (/=' ')) xpText
    xpSLC = xpWrapU (\(s, l, c) -> SLC {seeders=s, leechers=l, compl=c}) $ xpTriple xpSoLoC xpSoLoC xpSoLoC
    xpName = xpElem "td" xpText
    xpCat = xpWrapU (read . takeWhileEnd (/='=')) $ xpElem "td" $ xpAttr1 "a" ("href", xpText)
    xpSize = xpElem "td" xpText
    xpHash = xpElem "td" xpText
    xpUploader = xpElem "td" $ xpAlt (\case
      Anonymous -> 0
      Uploader _ _ -> 1) [xpWrapU (const Anonymous) xpText, xpWrapU (\(url, name) -> Uploader {upurl=url, upname=name}) $ xpAttr1Elem "a" ("href", xpText) xpText]
    xpDate = xpElem "td" $ xpPair xpText (xpElem "i" xpText)
    xpFile = xpWrapU (\(size, name) -> File {name=name, size=size}) $ xpElem "tr" $ xpPair (xpElem' "td" xpText) (xpElem' "td" xpText)
    xpFiles = xpElem' "tbody" $ xpList xpFile
    xpPres = xpElem' "section" xpXmlText
    xpCom = xpWrapU (\((avatar, role, (url, name), (up, down)), (age, content)) ->
      Commentary {user=User {userurl=url, avatarurl=avatar, username=name, role=role, upsize=up, downsize=down}, comage=age, comcontent=pack content}) $
      xpFilterCont (changeChildren (take 2)) $ xpElem' "li" $ xpPair
      (xpElem' "div" $ xp4Tuple
        (xpElem' "a" $ xpAttr1"img" ("src", xpText))
        (xpElemWithAttrValue "p" "class" "rang" xpText)
        (xpElemWithAttrValue "p" "class" "name" $ xpFilterCont (hasName "a") $ xpAttr1Elem "a" ("href", xpText) xpText)
        (xpElemWithAttrValue "p" "class" "ratio" $ xpFilterCont (hasName "strong") $ xpPair (xpElemWithAttrValue "strong" "class" "green" xpText) (xpElemWithAttrValue "strong" "class" "red" xpText)))
      (xpElem' "div" $ xpPair
        (xpElem' "div" $ xpFilterCont (processChildren (hasName "strong") >>> getChildren) $ xpElem "strong" xpText)
        (xpFilterCont (changeChildren (take 1)) $ xpElem' "div" $ xpElem' "span" xpXmlText))
    xpComs = xpElem' "ul" $ xpList xpCom

{-================================= DecodeMail =================================-}

decodeMail :: String -> String
decodeMail s =
  let (a:is) = fst . head . readHex <$> chunksOf 2 s
   in decode $ chr . xor a <$> is

decodeMails :: ArrowXml a => a XmlTree XmlTree
decodeMails = processTopDown $ (getAttrValue "data-cfemail" >>> arr decodeMail >>> mkText) `when` (isElem >>> hasAttrValue "class" (=="__cf_email__"))

{- xpMail :: PU String
xpMail =
  xpWrapU (\case
    Left s -> s
    Right s -> decodeMail s) $
  xpAlt (\case
    Left _ -> 0
    Right _ -> 1)
    [ xpWrapU Left xpText,
      xpWrapU Right $ xpFilterCont (isElem >>> hasAttrValue "class" (=="__cf_email__") >>> getAttrValue "data-cfemail" >>> mkText) xpText] -}
