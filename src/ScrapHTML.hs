{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ScrapHTML where

import Data.Bits (Bits (xor))
import Data.Char (chr, isDigit)
import Data.List
import Data.List.Extra (dropEnd, takeEnd, takeWhileEnd, trim)
import Data.List.Split (chunksOf, startsWith)
import Data.Text (Text, pack)
import Network.URI.Encode (decode)
import Numeric (readHex)
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Types
import Utils
import Config
import Data.Maybe

getdoc :: String -> IOStateArrow () XmlTree XmlTree
getdoc url = readDocument [withCurl [], withParseHTML True] url >>> removeAllWhiteSpace >>> decodeMails

{-================================= Search =================================-}

parseSize :: String -> Integer
parseSize s =
  let (v, u) = span (\c -> isDigit c || c == '.') s
      vn = readR v :: Rational
      um = case u of
        "o" -> 1
        "ko" -> 1000
        "Mo" -> 1000000
        "Go" -> 1000000000
        "To" -> 1000000000000
   in floor $ vn * um

readCat :: Int -> TorCat
readCat ic =
  let icr = if ic > 2188 then ic - 2148 else ic - 2147
  in toEnum icr

selectResultsTable :: ArrowXml a => a XmlTree XmlTree
selectResultsTable = mkelem "div" [] [ deep $
      hasName "main" >>> getChildreni (==0) /> processChildren (hasName "section") >>> (
            (getChildreni (==1) /> hasAttrValue "class" (=="pagination") >>> changeChildren (takeEnd 1) /> getChildren >>> getChildren)
        <+> (getChildren >>> hasAttrValue "id" (=="#torrents") >>> getChildreni (==1) /> getChildreni (==1)) ) ]

xpTF :: Config -> PU SearchResult 
xpTF c =
  xpWrapU (\(sm, resm) ->
    let eos = maybe True (\s -> not $ "suivante" `isPrefixOf` s || "dernière" `isPrefixOf` s) sm
        res = fromMaybe [] resm
     in SearchResult {endOfSearch=eos, searchResults=res}) $
    xpElem "div" $ xpPair (xpOption xpText) $
      xpOption $ xpElem "tbody" $ xpList $
        xpWrapU (\(cat, (url, name), tid, coms, age, size, compl, seeders, leechers) ->
          TorrentFile {fileinfo=FileInfo {name=trim name, size=parseSize size}, cat=readCat cat, torurlend=gettorurlend c url, tid=tid, coms=Just coms, age=trim age, slc=SLC {compl=compl, seeders=seeders, leechers=leechers}}) $
          xpElem "tr" $
            xp9Tuple
              (xpElem "td" $ xpFilterCont (hasName "div") $ xpElem' "div" xpRS) -- Section
              (xpElem' "td" $ xpAttr1Elem "a" ("href", xpText) xpTexts) -- URL & nom
              (xpElem "td" $ xpAttr1 "a" ("target", xpRS)) -- NFO
              (xpElem "td" $ xpFilterCont isText xpRS) -- Comments
              (xpElem "td" $ xpFilterCont isText xpText) -- Age
              (xpElem "td" xpText) -- Taille
              (xpElem "td" xpRS) -- Compl
              (xpElem "td" xpRS) -- Seed
              (xpElem "td" xpRS) -- Leech

{-================================= TorrentInfos =================================-}

gettorurlend :: Config -> String -> String
gettorurlend c = drop $ length (hostName c)

makefiletree :: [(String, FilePath)] -> [FileTree]
makefiletree files =
  let psfiles = second (break (=='/')) <$> files
      (psfilesf, psfilesd) = partition (null . snd . snd) psfiles
      dirs = (\files@((_, (p, _)):_) -> (p, (\(size, (_, s)) -> (size, tail s)) <$> files)) <$> groupBy' (fst . snd) psfilesd
      filestree = (\(size, (name, _)) -> File $ FileInfo {name=name, size=parseSize size}) <$> psfilesf
      dirstree = (\(p, f) -> Directory {dirname=p, dircontent=makefiletree f}) <$> dirs
  in dirstree ++ filestree

selectTI :: ArrowXml a => a XmlTree XmlTree -> a XmlTree XmlTree
selectTI files =
  root
    []
    [
      deep (
        hasName "main" /> getChildreni (== 1) >>> processChildren (hasName "section")
          >>> ( (getChildreni (== 0) >>> getChildreni (== 1) /> getChildren >>> changeChildren (takeEnd 2 . dropEnd 1)
                  >>> ( (getChildreni (== 0) >>> getChildreni odd >>> getChildren)
                          <+> (getChildreni (== 1) >>> getChildreni (== 1) >>> getChildreni (== 1) /> getChildren /> getChildreni (/= 1) >>> getChildreni (== 1))
                      )) -- Infos
                  <+> (getChildreni (== 1) >>> changeChildreni (== 2)) -- Presentation
                  <+> (getChildreni (== 3) /> getChildren >>> hasName "ul") -- Comments
              ))
          <+> (files /> getChildren) -- Content
          <+> (getChildren >>> hasName "html" /> hasName "head" >>> processChildren (filterA (getName >>> isA (/="script")))) -- Head
    ]

xpTI :: Config -> String -> String -> Int -> PU TorrentInfo
xpTI c nfo url tid =
  xpWrapU (\(slc, name, cat, size, hash, uploader, (dh, age), presentation, coms, files, header) ->
    let fullpres = runLA $ root [] [ {- mkelem "div" [] [
         mkelem "base" [mkAttr (mkName "href") (ac url >>> mkText)] [],
         ac header >>> getChildren,
         ac presentation ] -} ac presentation >>> processTopDownWithAttrl (changeAttrValue (hostName c ++) `when` (isAttr >>> hasName "href"))] >>> writeDocumentToString [withOutputHTML]
        [date, hour] = words . trim $ dh
    in TorrentInfo {baseinfo=TorrentFile {fileinfo=FileInfo {name=name, size=parseSize size}, cat=cat, torurlend = gettorurlend c url, tid=tid, coms=Nothing, age=drop 1 . dropEnd 1 . trim $ age, slc=slc}, hash=hash, content=files, nfo=nfo, uploader=uploader, date=date, hour=hour, presentation=mconcat $ fullpres (), comments=coms}) $
    xp11Tuple
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
      xpTree
  where
    xpSoLoC = xpElem' "strong" $ xpWrapU (read . filter (/=' ')) xpText
    xpSLC = xpWrapU (\(s, l, c) -> SLC {seeders=s, leechers=l, compl=c}) $ xpTriple xpSoLoC xpSoLoC xpSoLoC
    xpName = xpElem "td" xpTexts
    xpCat = xpWrapU (readCat . read . takeWhileEnd (/='=')) $ xpElem "td" $ xpAttr1 "a" ("href", xpText)
    xpSize = xpElem "td" xpText
    xpHash = xpElem "td" xpText
    xpUploader = xpElem "td" $ xpAlt (\case
      Nothing -> 1
      Just _ -> 0) [xpWrapU (\(url, name) -> Just $ Uploader {upurl=url, upname=name}) $ xpAttr1Elem "a" ("href", xpText) xpTexts, xpWrapU (const Nothing) xpTexts]
    xpDate = xpElem "td" $ xpPair xpText (xpElem "i" xpText)
    xpFile = xpElem "tr" $ xpPair (xpElem' "td" xpText) (xpElem' "td" xpText)
    xpFiles = xpElem' "tbody" $ xpWrapU makefiletree $ xpList xpFile
    xpPres = xpElem' "section" xpTree
    xpCom = xpWrapU (\((avatar, role, (url, name), (up, down)), (age, content)) ->
      Commentary {user=User {userurl=url, avatarurl=avatar, username=name, role=role, upsize=parseSize up, downsize=parseSize down}, comage=age, comcontent=content}) $
      xpFilterCont (changeChildren (take 2)) $ xpElem' "li" $ xpPair
      (xpElem' "div" $ xp4Tuple
        (xpElem' "a" $ xpAttr1 "img" ("src", xpText))
        (xpElemWithAttrValue "p" "class" "rang" xpText)
        (xpElemWithAttrValue "p" "class" "name" $ xpFilterCont (hasName "a") $ xpAttr1Elem "a" ("href", xpText) xpTexts)
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
decodeMails = processTopDown $ (getAttrValue "data-cfemail" >>> decodeMail ^>> mkText) `when` (isElem >>> hasAttrValue "class" (=="__cf_email__"))

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
