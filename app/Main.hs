{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import Options (getOptions, Options (optDbFile))
import Receiver (receive)
import ProcessMessages (tmpFilesProcessor)
import Database ( initializeDb )
  
import RIO ( ($), Bool(True), IO, newTMVarIO )
import Control.Concurrent (forkIO)

main :: IO ()
main = do
  opts <- getOptions

  initializeDb $ optDbFile opts
  
  -- Synchronizing mutex initialized with a value (any value) to ensure that
  -- pending messages are processed
  pendingFiles <- newTMVarIO True

  forkIO $ tmpFilesProcessor pendingFiles opts
  receive pendingFiles opts
  
