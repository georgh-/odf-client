{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Receiver (receive) where

import Options (Options, optPort, optTmpFolder)

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import RIO
import RIO.Time
import RIO.FilePath
import qualified RIO.Text as T
import qualified RIO.ByteString.Lazy as LBS

import Data.Conduit
import Data.Conduit.Combinators (withSinkFile)
import Network.Wai.Conduit

receive :: Options -> IO ()
receive ops = do
  let port = optPort ops
      tmpFolder = optTmpFolder ops
  
  run port (waiApp tmpFolder)

waiApp :: String -> Application
waiApp tmpFolder request respond = do
  timeReceived <- getZonedTime

  let path = T.concat $ pathInfo request
      method = requestMethod request

  case (method, path) of
    ("GET",  "")    -> respond $ mkResponse status200 helpText
    ("GET",  _)     -> respond $ mkResponse status404 ""
    ("POST", "")    -> respond $ mkResponse status200
                       "Message received correctly. This path \"/\" ignores it"
    ("POST", "odf") -> respond =<< writeBody timeReceived tmpFolder request
    (_,      _)     -> respond $ mkResponse status500 "Wrong method and path."

mkResponse :: Status -> LBS.ByteString -> Response
mkResponse status = responseLBS status [("Content-Type", "text/plain")]

writeBody :: ZonedTime -> String -> Request -> IO Response
writeBody timestamp tmpFolder request = do
  let
    timestampText = show timestamp
    filePath = tmpFolder </> timestampText

  withSinkFile filePath $ \dest ->
    runConduit
      $ sourceRequestBody request
     .| dest 
    
  pure $ mkResponse status200 "a"

helpText :: LBS.ByteString
helpText =
  "It works! This is the help page of the receiver\n\
  \Use POST /odf to write ODF messages to disk\n\
  \Use POST / to consume the HTTP request and ignore it\n\n\
  \In any case, it is assumed that the body contains the message \
  \directly, without specifying any parameter." 

