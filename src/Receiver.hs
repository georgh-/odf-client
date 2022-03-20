{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Receiver (receive) where

import Options (optPort, optTmpFolder)
import Files (genTmpFilePath)

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import RIO hiding (log)
import RIO.Time
import RIO.Directory (createDirectoryIfMissing)
import qualified RIO.Text as T
import qualified RIO.ByteString.Lazy as LBS

import Data.Conduit
import Data.Conduit.Combinators (withSinkFile, sinkNull)
import Network.Wai.Conduit
import App (envMsgsPending, askOpt, Env (envMsgsPending), runApp, App)
import Colog (log, Severity(Info))

-- Note: because App is not using tagless/mtl style (it's a naive monolythic
-- monad, simpler to define and use), we have to convert the Wai "run"
-- function from IO to APP by lifting and running the App as required instead
-- of IO. This ensures we have all the environment and logging facilities
-- provided by the App type.
--
-- These details are encapsulated in the "receive" function, the rest of the
-- module can be easily understood disregarding this function.
--
-- Summary:
--   normal waiApp :: Request -> (Response -> IO  ResponseReceived) -> IO  ResponseReceived
--   lifted server :: Request -> (Response -> App ResponseReceived) -> App ResponseReceived

receive :: App ()
receive = do
  port <- askOpt optPort
  env <- ask

  log Info $ T.pack $ "Starting ODF receiver on port " ++ show port
  liftIO $ run port (appLiftedServer env)

  where
    appLiftedServer env req res = runApp (server req $ liftedResponse res) env
    liftedResponse res = liftIO . res

server :: Request -> (Response -> App ResponseReceived) -> App ResponseReceived
server request respond = do
  timeReceived <- getZonedTime

  let
    path = T.concat $ pathInfo request
    method = decodeUtf8Lenient $ requestMethod request
    respondLog = loggedRespond timeReceived respond

  log Info $ T.concat ["Received request: ", method, " ", path]

  case method of
    "GET"  -> case path of
               "" -> respondLog $ mkResponse status200 helpText
               _  -> respondLog $ mkResponse status404 ""

    "POST" -> case path of
               ""    -> respondLog =<< liftIO (ignoreReq request)
               "odf" -> respondLog =<< processReq timeReceived request
               _     -> respondLog $ mkResponse status404 ""

    -- HTTP Specifies HEAD is mandatory
    "HEAD" -> respondLog $ mkResponse status200 ""
    _      -> respondLog $ mkResponse status405 "Only GET and POST are accepted."

mkResponse :: Status -> LBS.ByteString -> Response
mkResponse status = responseLBS status [("Content-Type", "text/plain")]

ignoreReq :: Request -> IO Response
ignoreReq request = do
  -- The full request must be consumed before responding, otherwise
  -- it would be reported as an error by the HTTP sender
  runConduit
    $ sourceRequestBody request
   .| sinkNull

  pure $ mkResponse
    status200
    "Message received correctly. This path \"/\" ignores it"

processReq :: ZonedTime -> Request -> App Response
processReq timestamp request = do
  pendingFiles <- asks envMsgsPending
  tmpFolder <- askOpt optTmpFolder

  let
    filePath = genTmpFilePath timestamp tmpFolder
    createParents = True

  createDirectoryIfMissing createParents tmpFolder
  writeBody filePath request

  _ <- atomically $ tryPutTMVar pendingFiles True

  pure $ mkResponse status200 "Message recived and saved"

writeBody :: FilePath -> Request -> App ()
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

elapsedTime :: ZonedTime -> App NominalDiffTime
elapsedTime initialTime = do
  finalTime <- getCurrentTime
  pure $ diffUTCTime finalTime $ zonedTimeToUTC initialTime

loggedRespond :: ZonedTime
              -> (Response -> App ResponseReceived)
              -> Response
              -> App ResponseReceived
loggedRespond receivedTime respond response = do
  processingTime <- elapsedTime receivedTime

  log Info $ T.pack $ concat
    [ "Processed request: "
    , show $ statusCode $ responseStatus response, " "
    , show $ statusMessage $ responseStatus response, " "
    , "Processing time: ", show processingTime]

  respond response

