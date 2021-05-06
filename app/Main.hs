module Main (main) where

import Options (getOptions)
import Receiver (receive)

import RIO

main :: IO ()
main = do
  opts <- getOptions
  receive opts
  
  
