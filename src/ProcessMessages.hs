{-# LANGUAGE OverloadedStrings #-}
module ProcessMessages (tmpFilesProcessor, processTmpFiles) where

import Files (parseTmpFilePath, genMsgFilePath)
import Options (Options, optTmpFolder, optMsgFolder)
import ODFHeader
import Parse (parseODFHeader, parseGZipHeader)

import RIO hiding (mapM_)
import RIO.FilePath (takeDirectory)
import RIO.Directory (createDirectoryIfMissing, renameFile)

import Data.Conduit ( (.|), runConduitRes, ConduitT, runConduit, awaitForever, yield )
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
   .| sinkProcessFile (optMsgFolder opts)

sinkProcessFile :: MonadIO m => FilePath -> ConduitT FilePath o m ()
sinkProcessFile msgFolder = mapM_ $ liftIO . processTmpFile msgFolder

processTmpFile :: FilePath -> FilePath -> IO ()
processTmpFile msgFolder tmpFile = do
  isCompressed <- isGzipCompressed tmpFile
  odfHeader <- extractODFHeader isCompressed tmpFile
  let
    mValidTmpFile = parseTmpFilePath tmpFile
    mDestPath = genMsgFilePath
                  <$> mValidTmpFile
                  <*> Just odfHeader
                  <*> Just isCompressed
                  <*> Just msgFolder

  case mDestPath of
    Just dstFile -> renameFileParents tmpFile dstFile
    Nothing      -> pure ()

extractODFHeader :: Bool -> FilePath -> IO ODFHeader
extractODFHeader isCompressed file = do
  let
    condUngzip =
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

  pure $ either
    (const False)
    (const True)
    parsed

renameFileParents :: FilePath -> FilePath -> IO ()
renameFileParents origFile destFile = do
  let
    destFolder = takeDirectory destFile
    createParents = True

  createDirectoryIfMissing createParents destFolder
  renameFile origFile destFile
