module Processor (tmpFilesProcessor, processTmpFiles) where

import App
import Files (parseTmpFilePath, genMsgFilePath, genErrFilePath)
import ODFHeader
import Parse (parseODFHeader, parseGZipHeader)

import RIO hiding (mapM_)
import RIO.FilePath (takeDirectory)
import RIO.Directory (createDirectoryIfMissing, renameFile)

import Data.Conduit ( (.|), runConduitRes, runConduit, awaitForever, yield )
import Data.Conduit.Combinators (mapM_, sourceDirectory, withSourceFile)
import Data.Conduit.Zlib (ungzip)
import Data.Conduit.Attoparsec (sinkParserEither)
import Options (optTmpFolder, Options (optErrFolder, optMsgFolder))


tmpFilesProcessor :: App ()
tmpFilesProcessor = do
  pendingFiles <- asks envMsgsPending
  
  forever $ do
    _ <- atomically $ takeTMVar pendingFiles
    processTmpFiles

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

  let destFile =
        case parseTmpFilePath tmpFile of
          Nothing ->
            genErrFilePath tmpFile errFolder

          Just validTmpFile ->
            genMsgFilePath validTmpFile odfHeader isCompressed msgFolder

  liftIO $ renameFileParents tmpFile destFile

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
