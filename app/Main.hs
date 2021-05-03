module Main (main) where

import Options (getOptions)
import Receiver (receive)

import RIO
import Prelude (print)

main :: IO ()
main = do
  opts <- getOptions
  receive opts
  
  
