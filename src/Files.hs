module Files
  ( ValidTmpFile(ValidTmpFile)
  , vfTimestamp
  , vfFilePath
  , genTmpFilePath
  , parseTmpFilePath
  , genMsgFilePath
  , genErrFilePath
  , timestampFormat
  , timestampFormatDb
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
timestampFormat = "%Y-%m-%d_%H-%M-%S.%q%z"

timestampFormatDb :: String
timestampFormatDb = "%Y-%m-%dT%H:%M:%S.%q%z"

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
      datePart = takeWhile (/= '~') fileName
      
  timestamp <- parseTimeM
    False -- Do not accept leading and trailing whitespace
    defaultTimeLocale
    timestampFormat
    datePart

  Just $ ValidTmpFile timestamp tmpFilePath

genMsgFilePath :: ODFHeader -> Bool -> FilePath -> ValidTmpFile -> FilePath
genMsgFilePath odfHeader isCompressed msgFolder (ValidTmpFile timestamp _) =
  let
    datePath =
      formatTime
        defaultTimeLocale
        messageFolderDateFormat
        timestamp
        
    timestampPart =
      formatTime
        defaultTimeLocale
        timestampFormat
        timestamp

    odfPart = genODFFileName odfHeader

    ext = if isCompressed then ".xml.gz" else ".xml"
    
    fileName = timestampPart <> "~" <> odfPart <> ext
  in
    msgFolder </> datePath </> fileName

genErrFilePath :: FilePath -> FilePath -> FilePath
genErrFilePath errFolder tmpFile =
  errFolder </> takeFileName tmpFile
