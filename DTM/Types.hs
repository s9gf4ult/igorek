module DTM.Types where

import Data.Word
import Data.Fixed
import qualified Data.Text as T

data DateTime = DateTime
                { dYear :: Word8
                , dMonth :: Word8
                , dDay :: Word8
                , dHour :: Word8
                , dMinute :: Word8
                , dSecond :: Word8
                }

data Header = Header
              { hMagicWords :: (Word8, Word8, Word8)
              , hStartDate :: DateTime
              , hEndDate :: DateTime
              , hPlot :: T.Text
              , hProduct :: T.Text
              , hDiameter :: Word16
              , hThickness :: Deci
              , hBlock :: Word8
              , hTemperature :: Word8
              , hLength :: Word16
              , hProdNumber :: Word16
              , hArg1 :: Word16
              , hArg2 :: Word16
              , hArg3 :: Word16
              , hArg4 :: Word16
              , hCommonArg :: Word16
              , hCheckSum :: Word8
              , hCalcualtedChSum :: Word8
              }

newtype Sensors = Sensors [(Word16, Word16, Word16, Word16)]

data FullData = FullData
                { fHeader :: Header
                , fSensors :: Sensors
                }
