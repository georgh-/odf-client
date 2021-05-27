{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import Options (getOptions)
import Receiver (receive)
import ProcessMessages
  
import RIO
import Control.Concurrent (forkIO)
import Parse

main :: IO ()
main = do
  opts <- getOptions

  -- Synchronizing mutex initialized with a value (any value) to ensure that
  -- pending messages are processed
  pendingFiles <- newTMVarIO True

  -- forkIO $ tmpFilesProcessor pendingFiles opts
  -- receive pendingFiles opts
  processTmpFiles opts
