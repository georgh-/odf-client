{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module GenerateStats (main) where

import Options (optMsgFolder, getOptions)
import ODFHeader
import Parse (parseODFHeader, parseGZipHeader)
import Files (timestampFormat)

import RIO hiding (mapM_)
import RIO.Directory (getFileSize)

import Data.Conduit ( (.|), runConduitRes, runConduit, awaitForever, yield )
import Data.Conduit.Combinators (mapM_, withSourceFile, sourceDirectoryDeep)
import Data.Conduit.Zlib (ungzip)
import Data.Conduit.Attoparsec (sinkParserEither)
import Prelude (putStrLn)
import Data.List (intercalate)
import RIO.Text (unpack)
import RIO.Time (defaultTimeLocale, formatTime, parseTimeM, ZonedTime)
import RIO.FilePath (takeFileName)


main = do
  opts <- getOptions
  processMessages  (optMsgFolder opts)

processMessages :: FilePath -> IO ()
processMessages msgFolder =
  runConduitRes
    $ sourceDirectoryDeep False msgFolder
   .| sinkProcessFile

--sinkProcessFile :: MonadIO m => ConduitT FilePath () m ()
sinkProcessFile = mapM_ $ liftIO . processMessage 

processMessage :: FilePath -> IO ()
processMessage msgFile = do
  odfHeader <- extractODFHeader msgFile
  fileSize <- getFileSize msgFile

  let fileName = takeFileName msgFile
      datePart = takeWhile (/= '~') fileName
      
      tst = parseTimeM
        False -- Do not accept leading and trailing whitespace
        defaultTimeLocale
        timestampFormat
        datePart :: Maybe ZonedTime

      odfFields = [ odfCompetitionCode
                  , odfDocumentCode   
                  , odfDocumentSubcode
                  , odfDocumentType   
                  , odfDocumentSubtype
                  , odfVersion        
                  , odfResultStatus   
                  , odfLanguage       
                  , odfFeedFlag       
                  , odfDateMessage    
                  , odfTimeMessage    
                  , odfLogicalDate    
                  , odfSource         
                  ]

  case tst of
    Nothing -> pure ()
    Just t -> do
      let odf  = map unpack $ odfFields <*> [odfHeader]
          date = formatTime defaultTimeLocale "%Y-%m-%d" t
          time = formatTime defaultTimeLocale "%H:%M:%S" t
          allData = date : time : odf ++ [show fileSize]
          
      putStrLn $ intercalate "\t" allData

extractODFHeader :: FilePath -> IO ODFHeader
extractODFHeader file = do
  mdwUnzip <- mdwConditionalUnzip file
  parsed <- withSourceFile file $ \src ->
    runConduit
      $ src
     .| mdwUnzip
     .| sinkParserEither parseODFHeader

  pure $ fromRight emptyODFHeader parsed

mdwConditionalUnzip file = do
  parsed <- withSourceFile file $ \src ->
    runConduit
      $ src
     .| sinkParserEither parseGZipHeader

  pure $ case parsed of
    Left  _ -> awaitForever yield
    Right _ -> ungzip

