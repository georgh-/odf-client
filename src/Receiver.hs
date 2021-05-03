{-# LANGUAGE OverloadedStrings #-}
module Receiver (receive) where

import Options (Options, optPort, optTmpFolder)

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import RIO
import RIO.Time
import qualified RIO.ByteString.Lazy as LBS
import qualified RIO.Text as T
--import Prelude (print, putStrLn)
  
receive :: Options -> IO ()
receive ops = do
  let port = optPort ops
      tmpFolder = optTmpFolder ops
  
  run port (waiApp tmpFolder)

waiApp :: String -> Application
waiApp tmpFolder request respond = do
  timeReceived <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

  let path = T.concat $ pathInfo request
      method = requestMethod request

  respond $ case (method, path) of
    ("GET",  "")    -> mkResponse status200 helpText
    ("GET",  _)     -> mkResponse status404 ""
    ("POST", "")    -> mkResponse status200
                       "Message received correctly. This path \"/\" ignores it"
    ("POST", "odf") -> mkResponse status200 "haahahha"
    (_, _)          -> mkResponse status500 "Wrong method and path."

mkResponse :: Status -> LBS.ByteString -> Response
mkResponse status = responseLBS status [("Content-Type", "text/plain")]

helpText :: LBS.ByteString
helpText = "It works! This is the help page of the receiver\n\
           \Use POST /odf to write ODF messages to disk\n\
           \Use POST / to consume the HTTP request and ignore it\n\n\
           \In any case, it is assumed that the body contains the message \
           \directly, without specifying any parameter." 

