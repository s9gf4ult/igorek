{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
, BangPatterns
  #-}

module Main where


import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans.Class
import DTM.CSV
import Data.Char (toLower)
import DTM.Generator
import DTM.Helpers
import DTM.Types
import Data.List (mapAccumL)
import Data.Maybe
import Data.Monoid
import Data.Serialize
import System.Console.GetOpt
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import qualified Data.Text.Lazy as T
import qualified Data.Vector as V

data COption = CHelp
             | CommonComp [CompRange]
             | SpecComp [CompRange]
             | Thickness Double
             | CFile FilePath
             | GapCount (Int, Int)
             | GapRatio (Double, Double)
             deriving (Show, Eq)

data DOption = DOption
               { dCommon :: [CompRange]
               , dSpec :: [CompRange]
               , dHigh :: Maybe Double
               , dFile :: Maybe FilePath
               , dGapCount :: Maybe (Int, Int)
               , dGapRatio :: Maybe (Double, Double)
               }
               deriving (Show, Eq)

mergeOptions :: [COption] -> DOption
mergeOptions v = DOption
                 { dCommon = concat $ cmn v
                 , dSpec = concat $ spec v
                 , dHigh = getLast $ mconcat $ map (Last . gethi) v
                 , dFile = getLast $ mconcat $ map (Last . getfile) v
                 , dGapCount = getLast $ mconcat $ map (Last . getgapc) v
                 , dGapRatio = getLast $ mconcat $ map (Last . getgapr) v
                 }
  where
    getgapc (GapCount x) = Just x
    getgapc _ = Nothing
    getgapr (GapRatio x) = Just x
    getgapr _ = Nothing
    cmn [] = []
    cmn ((CommonComp x):xs) = x:(cmn xs)
    cmn (_:xs) = cmn xs
    spec [] = []
    spec ((SpecComp x):xs) = x:(spec xs)
    spec (_:xs) = spec xs
    getfile (CFile f) = Just f
    getfile _ = Nothing
    gethi (Thickness d) = Just d
    gethi _ = Nothing

optionsDescr :: [OptDescr COption]
optionsDescr = [ Option "h" ["help"] (NoArg CHelp) "show this message"
               , Option "c" ["common"] (ReqArg (CommonComp . parseComp . T.pack) "arg") "common sinus component"
               , Option "s" ["specific"] (ReqArg (SpecComp . parseComp . T.pack) "arg") "specific sinus component"
               , Option "H" ["high"] (ReqArg (Thickness . read) "arg") "initial thickness, default is 0"
               , Option "f" ["file"] (ReqArg CFile "arg") "csv file with data to process"
               , Option "g" ["gap-count"] (ReqArg (GapCount . parseIntRange . T.pack) "arg") "gap count, range or value"
               , Option "G" ["gap-ratio"] (ReqArg (GapRatio . parseRatRange . T.pack) "arg") "gap ratio, range or value"
               ]


headerFromCSV :: CSVInput -> Header
headerFromCSV ci = Header
                   (2, 2, 1)
                   (ciStart ci) (ciEnd ci)
                   (ciPlot ci) (ciProduct ci)
                   (ciDiameter ci) (ciThickness ci)
                   (ciBlock ci) (ciTemperature ci)
                   0 (ciProductNumber ci) 0 0 0 0
                   0 0 0

fixOptions :: DOption -> Int -> CSVInput -> (Int, CSVInput)
fixOptions mopts !ac csv = (ac + 1, fixcsv)
  where
    fixcsv = csv { ciFile = ffixed
                 , ciHigh = hfixed
                 , ciCommon = cfixed
                 , ciSpecific = sfixed
                 }
    ffixed = case ciFile csv of
      Nothing -> Just $ show ac ++ ".dtm"
      Just f -> Just $ fixname f
    hfixed = case ciHigh csv of
      Nothing -> dHigh mopts
      Just h -> Just h
    cfixed = case ciCommon csv of
      Nothing -> Just $ dCommon mopts
      Just c -> Just $ c ++ dCommon mopts
    sfixed = case ciSpecific csv of
      Nothing -> Just $ dSpec mopts
      Just s -> Just $ s ++ dSpec mopts
    fixname x = case (map toLower $ reverse x) of
      "mtd." -> x
      _ -> x ++ ".dtm"


main :: IO ()
main = do
  (opts, _, _) <- (getOpt Permute optionsDescr) <$> getArgs
  case opts of
    (CHelp:_) -> putStrLn $ usageInfo "genData [options]" optionsDescr
    _ -> do
      let mopts = mergeOptions opts
      case dFile mopts of
        Nothing -> putStrLn "you need specify input file with option -f, use --help for help"
        Just filename -> do
          f <- BL.readFile filename
          case C.decodeByName f of
            Left e -> error e
            Right (_, v) -> do
              let (_, csvinps) =  mapAccumL (fixOptions mopts) 0 $ V.toList v
              g <- newStdGen
              (flip evalRandT) g $ forM_ csvinps $ \csv -> do
                (a, b, c, d) <- randomSensors
                                (fromIntegral $ ciLength csv)
                                (fromMaybe 0 $ ciHigh csv)
                                (fromMaybe [] $ ciCommon csv)
                                (fromMaybe [] $ ciSpecific csv)
                [aa, bb, cc, dd] <- map toRational <$> (replicateM 4 $ getRandomR (450, 470 :: Double))
                let sfd = genData
                          $ InitialData (aa, bb, cc, dd)
                          (map toRational a, map toRational b, map toRational c, map toRational d)
                          (headerFromCSV csv)
                fd <- addGaps (fromMaybe (0, 4) $ dGapCount mopts) (fromMaybe (5,15) $ dGapRatio mopts) sfd
                lift $ B.writeFile (fromMaybe "out.dtm" $ ciFile csv)
                  $ runPut $ genFullData fd
                lift $ putStrLn $ "file done: " ++ (fromMaybe "" $ ciFile csv)
