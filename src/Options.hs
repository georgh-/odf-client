module Options
  ( Options
  , optPort
  , optMsgFolder
  , getOptions)
where

import Options.Applicative
import Paths_odf_client (version)
import Data.Version (showVersion)

import RIO

data Options = Options
  { optPort :: !Int
  , optMsgFolder :: !String
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
  Options <$> port <*> msgFolder
  where
    port = option auto
      (short 'p'
        <> long "port"
        <> help "TCP Port to listen to"
        <> value 8080
        <> metavar "PORT")
      
    msgFolder = strOption
      (short 'f'
        <> long "msgFolder"
        <> help "Folder to store messages (default: ./messages)"
        <> value "./messages"
        <> metavar "PATH")

getOptions :: IO Options
getOptions = execParser optsParser
