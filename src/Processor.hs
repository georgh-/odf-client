{-# LANGUAGE OverloadedStrings #-}
module Processor (tmpFilesProcessor, processTmpFiles) where

import App
import Files (parseTmpFilePath, genMsgFilePath, genErrFilePath)
import ODFHeader
import Parse (parseODFHeader, parseGZipHeader)

import RIO hiding (mapM_, log)
import RIO.FilePath (takeDirectory)
import RIO.Directory (createDirectoryIfMissing, renameFile, getCurrentDirectory)

import Data.Conduit ( (.|), runConduitRes, runConduit, awaitForever, yield )
import Data.Conduit.Combinators (mapM_, sourceDirectory, withSourceFile)
import Data.Conduit.Zlib (ungzip)
import Data.Conduit.Attoparsec (sinkParserEither)
import Options (optTmpFolder, Options (optErrFolder, optMsgFolder))
import qualified RIO.Text as T
import Colog (log, Severity (Info))


tmpFilesProcessor :: App ()
tmpFilesProcessor = do
  pendingFiles <- asks envMsgsPending
  opts <- asks envOpts
  pwd <- liftIO getCurrentDirectory
  
  log Info $ T.pack $ concat
    [ "Messages processor started "
    , " workingDir: ", pwd
    , " tmpFolder: ", optTmpFolder opts
    , " msgFolder: ", optMsgFolder opts]
  
  forever $ do
    _ <- atomically $ takeTMVar pendingFiles
    _ <- log Info "Processing messages in tmp folder..."
    processTmpFiles
    log Info "Waiting for messages in tmp folder..."

processTmpFiles :: App ()
processTmpFiles = do
  tmpFolder <- askOpt optTmpFolder

  runConduitRes
    $ sourceDirectory tmpFolder
   .| mapM_ (lift . processTmpFile)

processTmpFile :: FilePath -> App ()
processTmpFile tmpFile = do
  isCompressed <- liftIO $ isGzipCompressed tmpFile
  odfHeader <- liftIO $ extractODFHeader isCompressed tmpFile
  errFolder <- askOpt optErrFolder
  msgFolder <- askOpt optMsgFolder

  log Info $ T.pack $ concat
    [ "Processing message: "
    , tmpFile
    ]

  let destFile =
        case parseTmpFilePath tmpFile of
          Nothing ->
            genErrFilePath tmpFile errFolder

          Just validTmpFile ->
            genMsgFilePath validTmpFile odfHeader isCompressed msgFolder

  liftIO $ renameFileParents tmpFile destFile

  log Info $ T.pack $ concat
    [ "Processed message: "
    , tmpFile
    , " Saved as: "
    , destFile
    ]

extractODFHeader :: Bool -> FilePath -> IO ODFHeader
extractODFHeader isCompressed file =
  let
    condUngzip =
      if isCompressed
        then ungzip
        else awaitForever yield

    parsed = withSourceFile file $ \src ->
      runConduit
        $ src
       .| condUngzip
       .| sinkParserEither parseODFHeader
  in
    fromRight emptyODFHeader <$> parsed

isGzipCompressed :: FilePath -> IO Bool
isGzipCompressed file =
  let parsed = withSourceFile file $ \src ->
        runConduit
          $ src
         .| sinkParserEither parseGZipHeader
  in
    isRight <$> parsed

renameFileParents :: FilePath -> FilePath -> IO ()
renameFileParents origFile destFile = do
  let
    destFolder = takeDirectory destFile
    createParents = True

  createDirectoryIfMissing createParents destFolder
  renameFile origFile destFile
