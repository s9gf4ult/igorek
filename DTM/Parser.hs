{-# LANGUAGE
  OverloadedStrings
  #-}  

module DTM.Parser where

import Control.Applicative
import DTM.Types
import Data.Monoid
import Data.Serialize
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Text as T

parseUntilEnd :: Get a -> Get [a]
parseUntilEnd f = (($[]) . appEndo) <$> (parseUntilEnd' $ Endo id)
  where
    parseUntilEnd' ac = do 
      remaining >>= \r -> case r of
        0 -> return ac
        _ -> do
          x <- f
          parseUntilEnd' $ ac <> Endo (x:)

parseDateTime :: Get DateTime
parseDateTime = do
  d <- getWord8
  m <- getWord8
  y <- getWord8
  h <- getWord8
  mn <- getWord8
  s <- getWord8
  return $ DateTime y m d h mn s


parseHeader :: Get Header
parseHeader = do
  b <- getByteString 127
  ch <- getWord8
  let cch = sum $ B.unpack b
  case runGet (parseHeaderHead cch ch) b of
    Left s -> fail s
    Right r -> return r

parseHeaderHead :: Word8 -> Word8 -> Get Header
parseHeaderHead cch ch = do
  Header
    <$> ((,,) <$> getWord8 <*> getWord8 <*> getWord8)
    <*> parseDateTime
    <*> parseDateTime
    <*> parseDTMtext
    <*> parseDTMtext
    <*> getWord16le             -- diam 
    <*> ((/10) . fromIntegral <$> getWord16le) -- thick
    <*> getWord8                                       -- block
    <*> getWord8                                       -- temper
    <*> (skip 1 >> getWord16le) -- length
    <*> getWord16le             -- prod num
    <*> (skip 2 >> getWord16le) -- arg1
    <*> getWord16le             -- arg2
    <*> getWord16le             -- arg3
    <*> getWord16le             -- arg4
    <*> getWord16le             -- common arg
    <*> return ch               -- chsum
    <*> return cch              -- calculated chsum
    

parseDTMtext :: Get T.Text
parseDTMtext = skip 32 >> return "undefined"

parseSensors :: Get Sensors
parseSensors = Sensors <$> get

parseFullData :: Get FullData
parseFullData = do
  FullData <$> parseHeader <*> parseSensors
