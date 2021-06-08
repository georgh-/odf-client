{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database
  ( initializeDb
  , ODFMessage
  , withMessagesDb
  , insertMessage
  , buildMessage
  )
where

import RIO

import Database.Beam
import Database.SQLite.Simple
import Database.Beam.Migrate
import Database.Beam.Sqlite
import Database.Beam.Migrate.Simple (VerificationResult(VerificationSucceeded,
                                                        VerificationFailed), verifySchema, createSchema)
import Database.Beam.Sqlite.Migrate (migrationBackend)
import Prelude (putStrLn)
import ODFHeader
import RIO.Time (ZonedTime)
import RIO.Text

data ODFMessageT f
  = ODFMessage
  { _msgCompetitionCode :: Columnar f Text
  , _msgDocumentCode    :: Columnar f Text
  , _msgDocumentSubcode :: Columnar f Text
  , _msgDocumentType    :: Columnar f Text
  , _msgDocumentSubtype :: Columnar f Text
  , _msgVersion         :: Columnar f Text
  , _msgResultStatus    :: Columnar f Text
  , _msgLanguage        :: Columnar f Text
  , _msgFeedFlag        :: Columnar f Text
  , _msgDateMessage     :: Columnar f Text
  , _msgTimeMessage     :: Columnar f Text
  , _msgLogicalDate     :: Columnar f Text
  , _msgSource          :: Columnar f Text
  , _msgReceivedTime    :: Columnar f Text
  , _msgFileSize        :: Columnar f Text
  , _msgFile            :: Columnar f Text
  } deriving (Generic, Beamable)

type ODFMessage = ODFMessageT Identity
deriving instance Show ODFMessage
deriving instance Eq ODFMessage
deriving instance Ord ODFMessage

type ODFMessageId = PrimaryKey ODFMessageT Identity


instance Table ODFMessageT where
  data PrimaryKey ODFMessageT f
    = ODFMessageId (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey = ODFMessageId . _msgReceivedTime

  
newtype MessagesDb f
  = MessagesDb
  { _odfMessages :: f (TableEntity ODFMessageT)
  } deriving (Generic, Database be)

messagesDb :: CheckedDatabaseSettings Sqlite MessagesDb
messagesDb = defaultMigratableDbSettings


-- https://www.fosskers.ca/en/blog/beam-migration
initializeDb :: FilePath -> IO ()
initializeDb dbFile =
  withMessagesDb dbFile $ \conn -> do

    runBeamSqlite conn $ do
      res <- verifySchema migrationBackend messagesDb
      case res of
        VerificationSucceeded -> pure ()
        VerificationFailed _  ->
          -- Create schema if it does not exist
          createSchema migrationBackend messagesDb

    execute_ conn "PRAGMA journal_mode='WAL'"

    execute_
      conn
      "CREATE UNIQUE INDEX IF NOT exists \
      \ msgTimestampIdx ON messages (received_time)"

    execute_
      conn
      "CREATE INDEX IF NOT exists \
      \ msgODFIDIdx ON messages \
      \  (competition_code \
      \  ,document_code \
      \  ,document_subcode \
      \  ,document_type \
      \  ,document_subtype \
      \  ,version \
      \  ,language \
      \  ,source)"

connectionSettings :: Connection -> IO ()
connectionSettings conn = do
  execute_ conn "PRAGMA synchronous='0'"
--  execute_ conn "PRAGMA wal_autocheckpoint='100'"

withMessagesDb :: FilePath -> (Connection -> IO a) -> IO a
withMessagesDb dbFile callback = 
  withConnection dbFile $ \conn ->
    connectionSettings conn >> callback conn

buildMessage :: ODFHeader -> String -> Integer -> FilePath -> ODFMessage
buildMessage header recTime fileSize path =
  ODFMessage
  { _msgCompetitionCode = odfCompetitionCode header
  , _msgDocumentCode    = odfDocumentCode    header
  , _msgDocumentSubcode = odfDocumentSubcode header
  , _msgDocumentType    = odfDocumentType    header
  , _msgDocumentSubtype = odfDocumentSubtype header
  , _msgVersion         = odfVersion         header
  , _msgResultStatus    = odfResultStatus    header
  , _msgLanguage        = odfLanguage        header
  , _msgFeedFlag        = odfFeedFlag        header
  , _msgDateMessage     = odfDateMessage     header
  , _msgTimeMessage     = odfTimeMessage     header
  , _msgLogicalDate     = odfLogicalDate     header
  , _msgSource          = odfSource          header
  , _msgReceivedTime    = pack recTime
  , _msgFileSize        = pack $ show fileSize
  , _msgFile            = pack path
  }

insertMessage :: Connection -> ODFMessage -> IO ()
insertMessage conn message = do
  runBeamSqlite conn $ runInsert $
    insert (_odfMessages $ unCheckDatabase messagesDb) $
      insertValues [message]
