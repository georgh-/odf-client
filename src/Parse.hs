{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parse (ODFHeader, parseODFHeader, parseGZipHeader) where

import RIO hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8 hiding (skipSpace)
import qualified Data.Attoparsec.ByteString as AB
import qualified RIO.HashMap as HM
import ODFHeader

parseGZipHeader :: Parser ()
parseGZipHeader = do
  AB.word8 0x1f
  AB.word8 0x8b
  pure ()

parseODFHeader :: Parser ODFHeader
parseODFHeader = 
  generateODFHeader <$> parseHeaderAttrs

generateODFHeader :: HM.HashMap ByteString ByteString -> ODFHeader
generateODFHeader attrsMap = 
  ODFHeader
    { odfCompetitionCode = getAttribute "CompetitionCode"
    , odfDocumentCode    = getAttribute "DocumentCode"
    , odfDocumentSubcode = getAttribute "DocumentSubcode"
    , odfDocumentType    = getAttribute "DocumentType"
    , odfDocumentSubtype = getAttribute "DocumentSubtype"
    , odfVersion         = getAttribute "Version"
    , odfResultStatus    = getAttribute "ResultStatus"
    , odfLanguage        = getAttribute "Language"
    , odfFeedFlag        = getAttribute "FeedFlag"
    , odfDateMessage     = getAttribute "Date"
    , odfTimeMessage     = getAttribute "Time"
    , odfLogicalDate     = getAttribute "LogicalDate"
    , odfSource          = getAttribute "Source"
    }
  where
    getAttribute key =
      decodeUtf8With lenientDecode $ HM.lookupDefault "" key attrsMap

parseHeaderAttrs :: Parser (HM.HashMap ByteString ByteString)
parseHeaderAttrs = do
  option "" $ string "<?xml"
  
  skipWhile (/= '<')
  string "<OdfBody"

  skipSpace
  HM.fromList <$> many parseAttribute

parseAttribute :: Parser (ByteString, ByteString)
parseAttribute = do
  skipSpace
  key <- takeWhile (\c -> not (isXMLSpace c) && (c /= '='))

  skipSpace
  char '='
  skipSpace

  char '"'
  val <- takeWhile (/= '"')
  char '"'

  pure (key, val)

skipSpace :: Parser ()
skipSpace = skipWhile isXMLSpace

isXMLSpace :: Char -> Bool
isXMLSpace ' '  = True
isXMLSpace '\t' = True
isXMLSpace '\r' = True
isXMLSpace '\n' = True
isXMLSpace _    = False

