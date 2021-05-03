module Options
  ( Options
  , optPort
  , optTmpFolder
  , optDstFolder
  , getOptions)
where

import Options.Applicative
import Paths_odf_client
import Data.Version (showVersion)

import RIO

data Options = Options
  { optPort :: !Int
  , optTmpFolder :: !String
  , optDstFolder :: !String
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
  Options <$> port <*> tmpFolder <*> dstFolder
  where
    port = option auto
      (short 'p'
        <> long "port"
        <> help "TCP Port to listen to"
        <> value 8080
        <> metavar "PORT")
      
    tmpFolder = strOption
      (short 't'
        <> long "tmpFolder"
        <> help "Folder to store messages before they are processed"
        <> value "./tmp"
        <> metavar "PATH")
      
    dstFolder = strOption
      (short 'd'
        <> long "dstFolder"
        <> help "Folder to store processed"
        <> value "./messages"
        <> metavar "PATH")


getOptions :: IO Options
getOptions = execParser optsParser
