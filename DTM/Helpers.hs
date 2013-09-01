
module DTM.Helpers where


import System.IO
import DTM.Types
import DTM.Parser
import DTM.Generator
import Data.Serialize
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL



makeMSensors :: ((Word16, Word16, Word16, Word16) -> (Word16, Word16, Word16, Word16)) -> (FullData -> FullData)
makeMSensors f = \x -> x {fSensors = ms $ fSensors x}
  where
    ms (Sensors a) = Sensors $ map f a

mapT1 :: (a -> a) -> ((a, b, c, d) -> (a, b, c, d))
mapT1 f = \(a, b, c, d) -> (f a, b, c ,d)
  

mapDTM :: FilePath -> (FullData -> FullData) -> IO ()
mapDTM fn maper = do
  b <- B.readFile fn
  case runGet parseFullData b of
    Left er -> fail er
    Right res -> do
      let new = maper res
      B.writeFile fn $ runPut $ genFullData new
