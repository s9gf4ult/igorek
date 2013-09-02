{-# LANGUAGE
  OverloadedStrings
  #-}

module Main where

import Control.Monad
import DTM.Generator
import DTM.Types
import DTM.Helpers
import Data.Time
import Data.Word
import Data.Serialize
import System.Environment
import qualified Data.Text.Lazy as T
import qualified Data.ByteString as B

grp4 :: [a] -> [(a,a,a,a)]
grp4 (a:b:c:d:xs) = (a,b,c,d):(grp4 xs)
grp4 [a,b,c] = [(a,b,c,c)]
grp4 [a,b] = [(a,b,b,b)]
grp4 [a] = [(a,a,a,a)]
grp4 [] = []

printHelp = putStrLn "usage: genSamples <low data> <high data> <low X> <high X>"

writeFiles :: [(FilePath, FullData)] -> IO ()
writeFiles x = forM_ x $ \(fp, fd) -> B.writeFile fp $ runPut $ genFullData fd

genFiles :: Word16 -> Word16 -> Word16 -> Word16 -> [(FilePath, FullData)]
genFiles lowd hid lowx hix = do
  x@(a, b, c, d) <- grp4 [lowx..hix]
  return (show a ++ "-" ++ show d ++ ".dtm", gfd [lowd..hid] x)
  where
    gfd d x = FullData (ghdr x) (gsns d)
    ghdr (x1, x2, x3, x4) = Header { hMagicWords = (2, 2, 1)
                                   , hStartDate = (DateTime 10 10 10 10 10 10)
                                   , hEndDate = (DateTime 10 10 10 10 11 10)
                                   , hPlot = ""
                                   , hProduct = ""
                                   , hDiameter = 720
                                   , hThickness = 14 --  FIXME: generate
                                   , hBlock = 129
                                   , hTemperature = 27
                                   , hLength = 1248
                                   , hProdNumber = 205
                                   , hArg1 = x1
                                   , hArg2 = x2
                                   , hArg3 = x3
                                   , hArg4 = x4
                                   , hCommonArg = 5920
                                   , hCheckSum = 0
                                   , hCalcualtedChSum = 0 }
    gsns a = Sensors $ map (\n -> (n,n,n,n)) a


main :: IO ()
main = do
  a <- getArgs
  case a of
    [lx, hx, llx, hhx] -> do
      let lowd = read lx
          hid = read hx
          lowx = read llx
          hix = read hhx
      when (lowd >= hid) $ fail "lowd must be < hid"
      when (lowx >= hix) $ fail "lowx must be < hix"
      writeFiles $ genFiles lowd hid lowx hix

    _ -> printHelp
