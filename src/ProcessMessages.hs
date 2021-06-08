{-# LANGUAGE OverloadedStrings #-}
module ProcessMessages (tmpFilesProcessor, processTmpFiles) where

import Files (parseTmpFilePath, genMsgFilePath, genErrFilePath, ValidTmpFile (vfTimestamp), timestampFormatDb)
import Options (Options (optDbFile), optTmpFolder, optMsgFolder, optErrFolder)
import ODFHeader
import Parse (parseODFHeader, parseGZipHeader)

import RIO hiding (mapM_)
import RIO.FilePath (takeDirectory)
import RIO.Directory (createDirectoryIfMissing, renameFile, getFileSize)

import Data.Conduit ( (.|), runConduitRes, runConduit, awaitForever, yield )
import Data.Conduit.Combinators (mapM_, sourceDirectory, withSourceFile)
import Data.Conduit.Zlib (ungzip)
import Data.Conduit.Attoparsec (sinkParserEither)
import Database (withMessagesDb, buildMessage, insertMessage)
import Database.SQLite.Simple (Connection)
import RIO.Time
import Prelude ( putStrLn, print )


tmpFilesProcessor :: TMVar Bool -> Options -> IO ()
tmpFilesProcessor pendingFiles opts =
  forever $ do
    print =<< getCurrentTime
    _ <- atomically $ takeTMVar pendingFiles
    processTmpFiles opts
    putStrLn "done"

processTmpFiles :: Options -> IO ()
processTmpFiles opts =
  withMessagesDb (optDbFile opts) $ \conn ->
    let sinkProcessFile =
          mapM_ $ liftIO . processTmpFile
                             conn
                             (optMsgFolder opts)
                             (optErrFolder opts)
    in
      runConduitRes
        $ sourceDirectory (optTmpFolder opts)
       .| sinkProcessFile

processTmpFile :: Connection -> FilePath -> FilePath -> FilePath -> IO ()
processTmpFile conn msgFolder errFolder tmpFile = do
  case parseTmpFilePath tmpFile of
    Nothing           -> tmpFileError errFolder tmpFile
    Just validTmpFile -> tmpFileValid conn validTmpFile msgFolder tmpFile

tmpFileError :: FilePath -> FilePath -> IO ()
tmpFileError tmpFile errFolder =
  let destFile = genErrFilePath tmpFile errFolder
  in renameFileParents tmpFile destFile

tmpFileValid :: Connection -> ValidTmpFile -> FilePath -> FilePath -> IO ()
tmpFileValid conn validTmpFile msgFolder tmpFile = do
  isCompressed <- isGzipCompressed tmpFile
  odfHeader <- extractODFHeader isCompressed tmpFile
  fileSize <- getFileSize tmpFile

  let recTime = formatTime
                  defaultTimeLocale
                  timestampFormatDb
                  (vfTimestamp validTmpFile)

      destFile = genMsgFilePath validTmpFile odfHeader isCompressed msgFolder

      message = buildMessage
                  odfHeader
                  recTime
                  fileSize
                  destFile

  renameFileParents tmpFile destFile

  insertMessage conn message

extractODFHeader :: Bool -> FilePath -> IO ODFHeader
extractODFHeader isCompressed file = do
  let condUngzip =
        if isCompressed
          then ungzip
          else awaitForever yield

  parsed <- withSourceFile file $ \src ->
    runConduit
      $ src
     .| condUngzip
     .| sinkParserEither parseODFHeader

  pure $ fromRight emptyODFHeader parsed

isGzipCompressed :: FilePath -> IO Bool
isGzipCompressed file = do
  parsed <- withSourceFile file $ \src ->
    runConduit
      $ src
     .| sinkParserEither parseGZipHeader

  pure $ isRight parsed

renameFileParents :: FilePath -> FilePath -> IO ()
renameFileParents origFile destFile = do
  let
    destFolder = takeDirectory destFile
    createParents = True

  createDirectoryIfMissing createParents destFolder
  renameFile origFile destFile
