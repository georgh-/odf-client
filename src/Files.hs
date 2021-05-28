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
timestampFormat = "%Y-%m-%d %H-%M-%S.%q %z"

messageFolderDateFormat :: String
messageFolderDateFormat = "%Y-%m-%d"

genTmpFilePath :: ZonedTime -> FilePath -> FilePath
genTmpFilePath timestamp tmpFolder =
  let
    fileName = formatTime
                 defaultTimeLocale
                 "%Y-%m-%d %H:%M:%S.%q %Z" --timestampFormat
                 timestamp
  in
    tmpFolder </> fileName

parseTmpFilePath :: FilePath -> Maybe ValidTmpFile
parseTmpFilePath tmpFilePath = do
  let fileName = takeFileName tmpFilePath

  timestamp <- parseTimeM
    False -- Do not accept leading and trailing whitespace
    defaultTimeLocale
    "%Y-%m-%d %H:%M:%S.%-q %Z" --timestampFormat
    fileName

  Just $ ValidTmpFile timestamp tmpFilePath

genMsgFilePath :: ValidTmpFile -> ODFHeader -> Bool -> FilePath -> FilePath
genMsgFilePath (ValidTmpFile timestamp _) odfHeader isCompressed msgFolder =
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

