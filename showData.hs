module Main where

import DTM.Parser
import DTM.Types
import DTM.Helpers
import Data.Time
import Data.Serialize
import Data.List (intercalate)
import System.Environment
import qualified Data.Text.Lazy as T
import qualified Data.ByteString as B



showDateTime :: DateTime -> [String]
showDateTime x = [show $ dtToLocalTime x]

showHeader :: Header -> [String]
showHeader h = indent [ show $ hMagicWords h ] 
               ++ (indent $ showDateTime $ hStartDate h)
               ++ (indent $ showDateTime $ hEndDate h)
               ++ indent [ "участок: " ++ (T.unpack $ hPlot h)
                         , "изделие: " ++ (T.unpack $ hProduct h)
                         , "диаметр: " ++ (show $ hDiameter h)
                         , "толщина: " ++ (show $ hThickness h)
                         , hshow "блок: " hBlock
                         , hshow "температура: " hTemperature
                         , hshow "длина: " hLength
                         , hshow "номер прибора: " hProdNumber
                         , hshow "x1: " hArg1
                         , hshow "x2: " hArg2
                         , hshow "x3: " hArg3
                         , hshow "x4: " hArg4
                         , hshow "y: " hCommonArg
                         ]
  where
    hshow pre f = pre ++ (show $ f h)
                    

showFull :: FullData -> [String]
showFull (FullData hd sn) = (showHeader hd)
                           ++ ["============================="]
                           ++ (showSensors sn)

showSensors :: Sensors -> [String]
showSensors (Sensors s) = map f $ zip [1..] s
  where
    f (n, (a, b, c, d)) = show n ++ ": "
                          ++ intercalate ", " [show a, show b, show c, show d]


indent :: [String] -> [String]
indent = map ("    "++)


main :: IO ()
main = do
  [f] <- getArgs
  b <- B.readFile f
  case runGet parseFullData b of
    Left er -> putStrLn er
    Right res -> mapM_ putStrLn $ showFull res
