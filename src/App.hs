{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module App (App(..), Env(..), runApp, askOpt) where

import Options ( Options )

import RIO
    ( Monad,
      Functor,
      Applicative,
      Bool,
      IO,
      asks,
      (.),
      MonadUnliftIO,
      MonadReader,
      MonadIO,
      ReaderT(..),
      TMVar )
import Colog (HasLog (..), LogAction, Message)

data Env m = Env 
  { envOpts :: !Options
  , envMsgsPending :: !(TMVar Bool)
  , envLogAction  :: !(LogAction m Message)
  }

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env { envLogAction = newLogAction }
  {-# INLINE setLogAction #-}


newtype App a = App
  { unApp :: ReaderT (Env App) IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Env App), MonadUnliftIO)

runApp :: App a -> Env App -> IO a
runApp app env = runReaderT (unApp app) env

-- Helper to get options from App without having to use asks
--  asks (optMsgFolder . envOpts) === askOpt optMsgFolder
askOpt :: (Options -> a) -> App a
askOpt opt = asks (opt . envOpts)

