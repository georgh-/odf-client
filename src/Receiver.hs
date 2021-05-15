{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Receiver (receive) where

import Options (Options, optPort, optMsgFolder)

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import RIO
import RIO.Time
import RIO.FilePath
import RIO.Directory (createDirectoryIfMissing)
import qualified RIO.Text as T
import qualified RIO.ByteString.Lazy as LBS

import Data.Conduit
import Data.Conduit.Combinators (withSinkFile)
import Network.Wai.Conduit

receive :: Options -> IO ()
receive ops = do
  let port = optPort ops
      msgFolder = optMsgFolder ops
  
  run port (waiApp msgFolder)

waiApp :: String -> Application
waiApp msgFolder request respond = do
  timeReceived <- getZonedTime

  let path = T.concat $ pathInfo request
      method = requestMethod request

  case (method, path) of
    ("GET",  "")    -> respond $ mkResponse status200 helpText
    ("GET",  _)     -> respond $ mkResponse status404 ""
    ("POST", "")    -> respond $ mkResponse status200
                       "Message received correctly. This path \"/\" ignores it"
    ("POST", "odf") -> respond =<< processReq timeReceived msgFolder request
    (_,      _)     -> respond $ mkResponse status500 "Wrong method and path."

mkResponse :: Status -> LBS.ByteString -> Response
mkResponse status = responseLBS status [("Content-Type", "text/plain")]

processReq :: ZonedTime -> FilePath -> Request -> IO Response
processReq timestamp msgFolder request = do
  let
    dirName = msgFolder </> formatTime defaultTimeLocale "%Y-%m-%d" timestamp
    fileName = show timestamp
    
    filePath = dirName </> fileName
    createParents = True

  createDirectoryIfMissing createParents dirName
  writeBody filePath request
  
  pure $ mkResponse status200 "Message recived and saved"

writeBody :: FilePath -> Request -> IO ()
writeBody filePath request =
  withSinkFile filePath $ \dest ->
    runConduit
      $ sourceRequestBody request
     .| dest 

helpText :: LBS.ByteString
helpText =
  "It works! This is the help page of the receiver\n\
  \Use POST /odf to write ODF messages to disk\n\
  \Use POST / to consume the HTTP request and ignore it\n\
  \n\
  \In any case, it is assumed that the body contains the message \
  \directly, without specifying any parameter." 

