{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import Options (getOptions)
import Receiver (receive)
import Processor (tmpFilesProcessor)
  
import RIO
import Control.Concurrent (forkIO)
import App (Env(Env, envOpts, envMsgsPending))

main :: IO ()
main = do
  opts <- getOptions

  -- Synchronizing mutex initialized with a value (any value) to ensure that
  -- pending messages are processed
  pendingFiles <- newTMVarIO True

  let env = Env
        { envOpts = opts
        , envMsgsPending = pendingFiles
        }

  forkIO $ runReaderT tmpFilesProcessor env
  receive pendingFiles opts
  
