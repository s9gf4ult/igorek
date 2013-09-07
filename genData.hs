{-# LANGUAGE
  OverloadedStrings
  #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import DTM.Generator
import DTM.Helpers
import DTM.Types
import Data.Fixed
import Data.List (genericLength, zip4)
import Data.Monoid
import Data.Maybe
import Data.Ratio
import Data.Serialize
import Data.Time
import Data.Word
import System.Console.GetOpt
import System.Environment
import System.Random
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as T

data COption = CHelp
             | CommonComp [CompRange]
             | SpecComp [CompRange]
             | Thickness Double
             deriving (Show, Eq)

data DOption = DOption
               { dCommon :: [CompRange]
               , dSpec :: [CompRange]
               , dHigh :: Double
               }
               deriving (Show, Eq)

mergeOptions :: [COption] -> DOption
mergeOptions x = DOption
                 { dCommon = concat $ cmn x
                 , dSpec = concat $ spec x
                 , dHigh = fromMaybe 0 $ hi x
                 }
  where
    cmn [] = []
    cmn ((CommonComp x):xs) = x:(cmn xs)
    cmn (_:xs) = cmn xs
    spec [] = []
    spec ((SpecComp x):xs) = x:(spec xs)
    spec (_:xs) = spec xs
    hi x = getLast $ mconcat $ map (Last . gethi) x
    gethi (Thickness d) = Just d
    gethi _ = Nothing

optionsDescr :: [OptDescr COption]
optionsDescr = [ Option "h" ["help"] (NoArg CHelp) "show this message"
               , Option "c" ["common"] (ReqArg (CommonComp . parseComp . T.pack) "arg") "common sinus component"
               , Option "s" ["specific"] (ReqArg (SpecComp . parseComp . T.pack) "arg") "specific sinus component"
               , Option "t" ["thickness"] (ReqArg (Thickness . read) "arg") "initial high of generated values, default is 0"
               ]


main = do
  (opts, _, _) <- (getOpt Permute optionsDescr) <$> getArgs 
  case opts of
    (CHelp:_) -> putStrLn $ usageInfo "genData [options]" optionsDescr
    _ -> do
      g <- newStdGen
      let mopts = mergeOptions opts
      
      let (a, b, c, d) = evalRand (randomSensors 500 (dHigh mopts) (dCommon mopts) (dSpec mopts)) g
      B.writeFile "out.dtm" $ runPut $ genFullData $ genData
        $ InitialData (470, 470, 470, 470)
        (map toRational a, map toRational b, map toRational c, map toRational d)
        $ Header (2, 2, 1) (DateTime 0 0 0 0 0 0) (DateTime 0 0 0 0 0 0) "" "" 500 17
        250 27 0 205 0 0 0 0 0 0 0
