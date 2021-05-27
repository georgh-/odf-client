{-# LANGUAGE OverloadedStrings #-}
module ODFHeader  where

import RIO hiding (takeWhile)
import RIO.Text (intercalate, unpack)

data ODFHeader = ODFHeader
  { odfCompetitionCode :: !Text
  , odfDocumentCode    :: !Text
  , odfDocumentSubcode :: !Text
  , odfDocumentType    :: !Text
  , odfDocumentSubtype :: !Text
  , odfVersion         :: !Text
  , odfResultStatus    :: !Text
  , odfLanguage        :: !Text
  , odfFeedFlag        :: !Text
  , odfDateMessage     :: !Text
  , odfTimeMessage     :: !Text
  , odfLogicalDate     :: !Text
  , odfSource          :: !Text
  } deriving Show

emptyODFHeader :: ODFHeader
emptyODFHeader = ODFHeader
  { odfCompetitionCode = ""
  , odfDocumentCode    = ""
  , odfDocumentSubcode = ""
  , odfDocumentType    = ""
  , odfDocumentSubtype = ""
  , odfVersion         = ""
  , odfResultStatus    = ""
  , odfLanguage        = ""
  , odfFeedFlag        = ""
  , odfDateMessage     = ""
  , odfTimeMessage     = ""
  , odfLogicalDate     = ""
  , odfSource          = ""
  }

genODFFileName :: ODFHeader -> String
genODFFileName odfHeader =
  let
    -- Only primary key fields included
    fields = 
      [ odfCompetitionCode
      , odfDocumentCode
      , odfDocumentSubcode
      , odfDocumentType
      , odfDocumentSubtype
      , odfVersion
      , odfResultStatus
      , odfLanguage
      , odfFeedFlag
      , odfSource
      ]
  in
    unpack $ intercalate "~" $ fields <*> pure odfHeader
