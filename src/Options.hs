module Options
  ( Options
  , optPort
  , optTmpFolder
  , optMsgFolder
  , optErrFolder
  , optDbFile
  , getOptions)
where

import Options.Applicative
import Paths_odf_client (version)
import Data.Version (showVersion)

import RIO

data Options = Options
  { optPort :: !Int
  , optTmpFolder :: !String
  , optMsgFolder :: !String
  , optErrFolder :: !String
  , optDbFile    :: !String
  } deriving Show

optsParser :: ParserInfo Options
optsParser = info options description
  where
    options = helper <*> versionOption <*> programOptions
    description = fullDesc
                  <> progDesc "ODF Client example"
                  <> header "odf-client - a small example ODF client"

versionOption :: Parser (a -> a)
versionOption = infoOption
  (showVersion version)
  (short 'v'
    <> long "version"
    <> help "Show version")

programOptions :: Parser Options
programOptions =
  Options <$> port <*> tmpFolder <*> msgFolder <*> errFolder <*> dbFile
  where
    port = option auto
      (short 'p'
        <> long "port"
        <> help "TCP Port to listen to (default: 8080)"
        <> value 8080
        <> metavar "PORT")
      
    tmpFolder = strOption
      (short 'd'
        <> long "temp-dir"
        <> help "Directory to use for temporary messages (default: ./tmp)"
        <> value "./tmp"
        <> metavar "PATH")

    msgFolder = strOption
      (short 'm'
        <> long "message-dir"
        <> help "Directory to use for messages (default: ./messages)"
        <> value "./messages"
        <> metavar "PATH")

    errFolder = strOption
      (short 'e'
        <> long "errors-dir"
        <> help "Directory used to store unkown files found in tmp folder\
                \(files not genereated by this program) (default: ./error)"
        <> value "./error"
        <> metavar "PATH")

    dbFile = strOption
      (long "db-file"
        <> help "Database file to use) (default: ./messagesDb.sqlite)"
        <> value "./messagesDb.sqlite"
        <> metavar "FILE_PATH")

getOptions :: IO Options
getOptions = execParser optsParser
