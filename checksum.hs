{-#
  LANGUAGE
  ScopedTypeVariables
  #-}

module Main where

import Control.Applicative
import Foreign.Storable
import Data.Binary
import Data.Binary.Get
import Data.Bits
import Numeric
import System.Environment
import qualified Data.ByteString.Lazy as B

readBlocks :: forall a. (Storable a) => Get a -> Get (Maybe [a])
readBlocks r = readBlocks' []
  where
    asize = fromIntegral $ sizeOf (undefined :: a)
    readBlocks' ac = do
      re <- remaining
      case re of
        0 -> return $ Just ac
        _ | re >= asize -> do
          b <- r
          readBlocks' $ ac ++ [b]
          | otherwise -> return Nothing

-- | remove each element from the list and return this element and remaining
-- list
blockOf :: (Num i) => [a] -> [(i, a, [a])]
blockOf x = bof 0 [] [] x
  where
    bof _ ac _ [] = ac
    bof nmb ac hd (a:as) = bof (nmb+1) ((nmb, a, hd ++ as):ac) (hd ++ [a]) as

-- | Get indexes where checksum eq a
getChsumIdx :: (Eq a) => [(i, a, [a])] -> ([a] -> a) -> [i]
getChsumIdx l chfun = map (\(i, _, _) -> i) $ filter (\(_, a, as) -> a == chfun as) l


findChsums :: forall a.(Storable a, Eq a) => B.ByteString -> [(Get a, String)] -> [([a] -> a, String)] -> [(Integer, String, String)]
findChsums bs rdrs chsms = do
  (rd, rdname) <- rdrs
  let blkcs = runGet (readBlocks rd) bs
  case blkcs of
    Nothing -> []
    Just blks -> do 
      (chsm, chname) <- chsms
      i <- getChsumIdx (blockOf blks) chsm
      return (i * (fromIntegral $ sizeOf (undefined :: a)), rdname, chname)

shex :: (Integral a, Show a) => a -> String
shex a = (showHex a) ""

formatResult :: (Integral i, Show i) => (i, String, String) -> String
formatResult (i, rdname, chname) = rdname ++ " found "
                                   ++ chname ++ " at " ++ shex i
          

main :: IO ()
main = do
  [f] <- getArgs
  putStrLn $ "File name: " ++ f
  bs <- B.take 128 <$> B.readFile f
  mapM_ putStrLn $ map formatResult $ findChsums bs [(getWord8, "Byte")] chsms
  mapM_ putStrLn $ map formatResult $ findChsums bs [(getWord16le, "Word le"),
                                                     (getWord16be, "Word be")] chsms
  mapM_ putStrLn $ map formatResult $ findChsums bs [(getWord32le, "DWord le"),
                                                     (getWord32be, "DWord be")] chsms
  where
    chsms :: (Num a, Bits a) => [([a] -> a, String)]
    chsms = [(sum, "sum"),
             (foldl xor 0, "xor")]

