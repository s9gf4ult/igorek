{-# LANGUAGE
  OverloadedStrings
  #-}

module Main where

import Control.Monad
import DTM.Generator
import DTM.Helpers
import DTM.Types
import Data.Fixed
import Data.Serialize
import Data.List (genericLength, zip4)
import Data.Time
import Data.Word
import Data.Ratio
import System.Environment
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as T

data InitialData = InitialData { iY :: (Rational, Rational, Rational, Rational)
                               , iZ :: ([Rational], [Rational], [Rational], [Rational])
                               , iHeader :: Header }


magicA, magicB :: Rational
magicA = 369 % 2500
magicB = (-37) % 1250

genX :: Rational -> [Rational] -> [Rational]
genX y zs = map f zs
  where
    f z = (z/magicA) - (y * magicB/magicA)

genData :: InitialData -> FullData
genData (InitialData
         (y1, y2, y3, y4)
         (z1, z2, z3, z4)
         hdr) = FullData
                hdr { hLength = (genericLength sensors)*2 - 2
                    , hArg1 = round y1
                    , hArg2 = round y2
                    , hArg3 = round y3
                    , hArg4 = round y4
                    , hCommonArg = 5920 }
                $ Sensors sensors
  where
    sensors = zip4
              (map round $ genX y1 z1)
              (map round $ genX y2 z2)
              (map round $ genX y3 z3)
              (map round $ genX y4 z4)

main = B.writeFile "out.dtm"
       $ runPut $ genFullData
       $ genData
       $ InitialData
       (470, 400, 400, 450)
       (mapsin 0 10, mapsin (ratpi/4) 10, mapsin (ratpi/2) 10, mapsin (ratpi*3/4) 10)
       $ Header
       (2, 2, 1)
       (DateTime 10 10 10 10 10 10)
       (DateTime 10 10 10 10 11 10)
       "жопа"
       "говно"
       720 14 104 27 0 205 0 0 0 0
       0 0 0
  where
    mapsin start len = map (\x -> toRational $ 12 + (sin $ fromRational $ 2*x)*2 - 0.1) [start,start+0.1..start+len]
    ratpi = toRational pi
