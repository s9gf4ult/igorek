module DTM.Types where

import Data.Word

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
              , hPlot :: Text
              , hProduct :: Text
              , hDiameter :: Word16
              , hThickness :: Deci
              , hBlock :: Word8
              , hTemperature :: Word8
              , hLength :: Word16
              , hProdNumber :: Word16
              , hCheckSum :: Word8
              , hCalcualtedChSum :: Word8
              }
