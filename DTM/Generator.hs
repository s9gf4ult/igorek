module DTM.Generator where

import Codec.Text.IConv (convert)
import Control.Applicative
import DTM.Types
import Data.Monoid
import Data.Serialize
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL


genText :: Putter TL.Text
genText t = putLazyByteString bs
  where
    b = BL.take 31 $ convert "utf-8" "cp1251" $ TL.encodeUtf8 t
    bl = fromIntegral $ BL.length b
    bs = b <> (BL.pack $ replicate (32 - bl) 0)


genFullData :: Putter FullData
genFullData (FullData hd sn) = genHeader hd >> genSensors sn

genDateTime :: Putter DateTime
genDateTime (DateTime y m d h mn sec) = do
  putWord8 d
  putWord8 m
  putWord8 y
  putWord8 h
  putWord8 mn
  putWord8 sec

genRawHeader :: Putter Header
genRawHeader h = do
  let (a, b, c) = hMagicWords h
  putWord8 a
  putWord8 b
  putWord8 c
  genDateTime $ hStartDate h
  genDateTime $ hEndDate h
  genText $ hPlot h
  genText $ hProduct h
  putWord16le $ hDiameter h
  putWord16le $ round $ 10 * hThickness h
  putWord8 $ hBlock h
  putWord8 $ hTemperature h
  putWord8 4
  putWord16le $ hLength h
  putWord16le $ hProdNumber h
  putWord16le 0
  putWord16le $ hArg1 h
  putWord16le $ hArg2 h
  putWord16le $ hArg3 h
  putWord16le $ hArg4 h
  putWord16le $ hCommonArg h

genHeader :: Putter Header
genHeader h = do
  putLazyByteString res
  putWord8 $ sum $ B.unpack b
  where 
    res = BL.fromChunks [b, B.pack $ replicate (127 - bl) 0]
    b = B.take 127 $ runPut $ genRawHeader h
    bl = fromIntegral $ B.length b

genSensors :: Putter Sensors
genSensors (Sensors sns) = mapM_ putCh sns
  where
    putCh (a, b, c, d) = do
      putWord16le a
      putWord16le b
      putWord16le c
      putWord16le d
