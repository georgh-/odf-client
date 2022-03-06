module App (App, Env(..), askOpt) where

import Options (Options)

import RIO

data Env = Env
  { envOpts :: Options
  , envMsgsPending :: TMVar Bool
  }

type App a = ReaderT Env IO a 

-- Helper to get options from App without having to use asks
--  optMsgFolder <$> asks opts === askOpt optMsgFolder
askOpt :: (Options -> a) -> App a
askOpt opt = opt <$> asks envOpts

