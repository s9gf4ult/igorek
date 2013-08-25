module Main where

import Control.Monad
import GHC.IO.Handle
import System.Environment
import System.IO
import qualified Data.ByteString as B

choffset :: (Num a) => a
choffset = 127                  -- magic, dont tuch

-- | Fix checksum in a header of *.dtm files. Arguments is a list of file names.
main :: IO ()
main = do
  fls <- getArgs
  forM_ fls $ \f -> withFile f ReadWriteMode $ \h -> do
    b <- B.hGet h choffset
    let res = sum $ B.unpack b
    hSeek h AbsoluteSeek choffset
    B.hPut h $ B.pack [res]
