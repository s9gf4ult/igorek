{-# LANGUAGE
  OverloadedStrings
  #-}

module DTM.CSV where


import Control.Applicative
import DTM.Helpers
import DTM.Types
import Data.Attoparsec.Text
import Data.Csv ((.:), FromNamedRecord(..))
import Data.Fixed
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL


data CSVInput = CSVInput
                { ciFile :: Maybe FilePath
                , ciStart :: DateTime
                , ciEnd :: DateTime
                , ciPlot :: TL.Text
                , ciProduct :: TL.Text
                , ciDiameter :: Word16
                , ciThickness :: Deci
                , ciBlock :: Word8
                , ciTemperature :: Word8
                , ciLength :: Word16
                , ciProductNumber :: Word16
                , ciHigh :: Maybe Double
                , ciCommon :: Maybe [CompRange]
                , ciSpecific :: Maybe [CompRange]
                } deriving (Show, Eq)

instance FromNamedRecord CSVInput where
  parseNamedRecord h = CSVInput
                         <$> (notempty (T.unpack . T.decodeUtf8) <$> h .: "file")
                         <*> (parseDate <$> h .: "start")
                         <*> (parseDate <$> h .: "end")
                         <*> ((TL.decodeUtf8 . BL.fromChunks . (:[])) <$> h .: "plot")
                         <*> ((TL.decodeUtf8 . BL.fromChunks . (:[])) <$> h .: "product")
                         <*> (parseNum <$> h .: "diameter")
                         <*> (parseRat <$> h .: "thickness")
                         <*> (parseNum <$> h .: "block")
                         <*> (parseNum <$> h .: "temperature")
                         <*> (parseNum <$> h .: "length")
                         <*> (parseNum <$> h .: "productnumber")
                         <*> (notempty parseRat <$> h .: "high")
                         <*> (notempty parseComps <$> h .: "common")
                         <*> (notempty parseComps <$> h .: "specific")
    where
      parseNum x = case parseOnly decimal (T.decodeUtf8 x) of
        Left e -> error $ "parseNum failed to parse:\"" ++ (T.unpack $ T.decodeUtf8 x) ++ "\" " ++ e
        Right r -> r
      parseRat x = case parseOnly rational (T.decodeUtf8 x) of
        Left e -> error $ "parseRat failed: \"" ++ (T.unpack $ T.decodeUtf8 x) ++ "\" " ++ e
        Right r -> r
      notempty f x = case parseOnly (skipSpace >> endOfInput) (T.decodeUtf8 x) of
        Left _ -> Just $ f x
        Right _ -> Nothing
      parseDate :: B.ByteString -> DateTime
      parseDate x = case parseOnly dateAttoParser (T.decodeUtf8 x) of
        Left e -> error $ "could not parse date from string \"" ++ (T.unpack $ T.decodeUtf8 x) ++ "\" " ++ e
        Right d -> d
      parseComps :: B.ByteString -> [CompRange]
      parseComps x = case parseOnly compParser (T.decodeUtf8 x) of
        Left e -> error e
        Right r -> r

dateAttoParser :: Parser DateTime
dateAttoParser = do
  y <- decimal
  dash
  m <- decimal
  dash
  d <- decimal
  skipSpace
  h <- decimal
  sem
  mn <- decimal
  sem
  sc <- decimal
  return $ DateTime (fromInteger $ y - 2000) m d h mn sc
  

  where
    dash = (skipSpace >> (char '-' <|> char '.') >> skipSpace)
    sem = (skipSpace >> char ':' >> skipSpace)
