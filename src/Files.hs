module Files
  ( ValidTmpFile
  , vfTimestamp
  , vfFilePath
  , genTmpFilePath
  , parseTmpFilePath
  , genMsgFilePath
  )
where

import RIO
import RIO.Time
import RIO.FilePath
import Parse
import ODFHeader (genODFFileName)

data ValidTmpFile = ValidTmpFile
  { vfTimestamp :: !ZonedTime
  , vfFilePath :: !FilePath
  } deriving Show

timestampFormat :: String
timestampFormat = "%Y-%m-%d %H:%M:%S.%-q %Z"

messageFolderDateFormat :: String
messageFolderDateFormat = "%Y-%m-%d"

genTmpFilePath :: ZonedTime -> FilePath -> FilePath
genTmpFilePath timestamp tmpFolder =
  let
    fileName = formatTime
                 defaultTimeLocale
                 timestampFormat
                 timestamp
  in
    tmpFolder </> fileName

parseTmpFilePath :: FilePath -> Maybe ValidTmpFile
parseTmpFilePath tmpFilePath = do
  let fileName = takeFileName tmpFilePath

  timestamp <- parseTimeM
    False -- Do not accept leading and trailing whitespace
    defaultTimeLocale
    timestampFormat
    fileName

  Just $ ValidTmpFile timestamp tmpFilePath

genMsgFilePath :: ValidTmpFile -> ODFHeader -> FilePath -> FilePath
genMsgFilePath (ValidTmpFile timestamp filePath) odfHeader msgFolder =
  let
    datePart = formatTime
                 defaultTimeLocale
                 messageFolderDateFormat
                 timestamp
    odfPart = genODFFileName odfHeader
    fileName = takeFileName filePath <> "~" <> odfPart
  in
    msgFolder </> datePart </> fileName

