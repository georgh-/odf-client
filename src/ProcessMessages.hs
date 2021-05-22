{-# LANGUAGE OverloadedStrings #-}
module ProcessMessages (tmpFilesProcessor) where

import Options (Options, optTmpFolder, optMsgFolder)
import Files (parseTmpFilePath, genMsgFilePath)

import RIO hiding (mapM_)
import RIO.Directory (createDirectoryIfMissing, renameFile)

import Data.Conduit ( (.|), runConduitRes, ConduitT )
import Data.Conduit.Combinators (mapM_, sourceDirectory)
import RIO.FilePath (takeDirectory)

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
  let
    maybeValidTmpFile = parseTmpFilePath tmpFile
    maybeDestPath = genMsgFilePath <$> maybeValidTmpFile <*> Just msgFolder

  maybe
    (pure ())
    (renameFileParents tmpFile)
    maybeDestPath

renameFileParents :: FilePath -> FilePath -> IO ()
renameFileParents origFile destFile = do
  let
    destFolder = takeDirectory destFile
    createParents = True

  createDirectoryIfMissing createParents destFolder
  renameFile origFile destFile
