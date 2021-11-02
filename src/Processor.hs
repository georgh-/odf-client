{-# LANGUAGE OverloadedStrings #-}
module Processor (tmpFilesProcessor, processTmpFiles) where

import Files (parseTmpFilePath, genMsgFilePath, genErrFilePath)
import Options (Options, optTmpFolder, optMsgFolder, optErrFolder)
import ODFHeader
import Parse (parseODFHeader, parseGZipHeader)

import RIO hiding (mapM_)
import RIO.FilePath (takeDirectory)
import RIO.Directory (createDirectoryIfMissing, renameFile)

import Data.Conduit ( (.|), runConduitRes, runConduit, awaitForever, yield )
import Data.Conduit.Combinators (mapM_, sourceDirectory, withSourceFile)
import Data.Conduit.Zlib (ungzip)
import Data.Conduit.Attoparsec (sinkParserEither)


tmpFilesProcessor :: TMVar Bool -> Options -> IO ()
tmpFilesProcessor pendingFiles opts =
  forever $ do
    _ <- atomically $ takeTMVar pendingFiles
    processTmpFiles opts

processTmpFiles :: Options -> IO ()
processTmpFiles opts =
  runConduitRes
    $ sourceDirectory (optTmpFolder opts)
   .| sinkProcessFile (optMsgFolder opts) (optErrFolder opts)

  where
    sinkProcessFile msgFolder errFolder =
      mapM_ $ liftIO . processTmpFile msgFolder errFolder

processTmpFile :: FilePath -> FilePath -> FilePath -> IO ()
processTmpFile msgFolder errFolder tmpFile = do
  isCompressed <- isGzipCompressed tmpFile
  odfHeader <- extractODFHeader isCompressed tmpFile

  let destFile =
        case parseTmpFilePath tmpFile of
          Nothing ->
            genErrFilePath tmpFile errFolder

          Just validTmpFile ->
            genMsgFilePath validTmpFile odfHeader isCompressed msgFolder

  renameFileParents tmpFile destFile

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
