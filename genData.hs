{-# LANGUAGE
  OverloadedStrings
  #-}

module Main where

import Control.Monad
import Control.Applicative
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
import System.Console.GetOpt
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as T

data COption = CHelp
             | CommonComp [SinComponent]
             | SpecComp [SinComponent]
             | High Double
             deriving (Show, Eq)

data DOption = DOption
               { dCommon :: [SinComponent]
               , dSpec :: [SinComponent]
               , dHigh :: Maybe Double
               }

mergeOptions :: [COption] -> DOption
mergeOptions x = DOption
                 { dCommon = concat $ cmn x
                 , dSpec = concat $ spec x
                 , dHigh = hi x
                 }
  where
    cmn [] = []
    cmn (CommonComp x):xs = x:(cmn xs)
    cmn _:xs = cmn xs
    spec [] = []
    spec (SpecComp x):xs = x:(spec xs)
    spec _:xs = spec xs
    hi x = getLast $ map (Last . gethi) x
    gethi (High d) = Just d
    gethi _ = Nothing

optionsDescr :: [OptDescr COption]
optionsDescr = [ Option "h" ["help"] (NoArg CHelp) "show this message"
               , Option "c" ["common"] (ReqArg (CommonComp . parseComp . T.pack) "arg") "common sinus component"
               , Option "s" ["specific"] (ReqArg (SpecComp . parseComp . T.pack) "arg") "specific sinus component"
               , Option "H" ["high"] (ReqArg (High . read) "arg") "initial high of generated values, default is 0"
               ]


main = do
  (opts, _, _) <- (getOpt Permute optionsDescr) <$> getArgs 
  case opts of
    (CHelp:_) -> putStrLn $ usageInfo "genData [options]" optionsDescr
    _ -> do
      g <- newStdGen
      let mopts = mergeOptions opts
      let sns = evalRand (randomSensors (dCommon mopts) (dSpec mopts)) g
      
