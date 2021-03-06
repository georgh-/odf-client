module Files
  ( ValidTmpFile
  , vfTimestamp
  , vfFilePath
  , genTmpFilePath
  , parseTmpFilePath
  , genMsgFilePath
  , genErrFilePath
  , timestampFormat
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

-- Used in both tmp files and processed files, to allow reprocessing of
-- messages by moving them to the tmp folder
timestampFormat :: String
timestampFormat = "%Y-%m-%d_%H-%M-%S.%q%z"

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
parseTmpFilePath tmpFilePath =
  let
    fileName = takeFileName tmpFilePath

    -- Allow reprocessing messages by moving them to tmp folder
    datePart = takeWhile (/= '~') fileName
      
    timestampM = parseTimeM
      False -- Do not accept leading and trailing whitespace
      defaultTimeLocale
      timestampFormat
      datePart
  in
    ValidTmpFile <$> timestampM <*> pure tmpFilePath

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

genErrFilePath :: FilePath -> FilePath -> FilePath
genErrFilePath tmpFile errFolder =
  errFolder </> takeFileName tmpFile
