
module DTM.Helpers where


import DTM.Generator
import DTM.Parser
import DTM.Types
import Data.Serialize (runGet, runPut)
import Data.Time
import Data.Word
import Data.Ratio
import Data.Attoparsec.Text.Lazy  
import Data.List (zip4, genericLength)
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

data SinComponent = SinComponent
                    { scPeriod :: Double
                    , scAmplitude :: Double }
                  deriving (Show, Eq)



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


dtToLocalTime :: DateTime -> LocalTime 
dtToLocalTime (DateTime y m d h mm sec) = LocalTime
                                          (fromGregorian
                                           (2000 + toInteger y)
                                           (fromIntegral m)
                                           (fromIntegral d))
                                          (TimeOfDay
                                           (fromIntegral h)
                                           (fromIntegral mm)
                                           (fromIntegral sec))


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
                                          
parseComp :: TL.Text -> [SinComponent]
parseComp t = case eitherResult $ parse compParser t of
  Left e -> error "sinus component must be in format: period:amplitude,period:amplitude..."
  Right r  -> r

compParser :: Parser [SinComponent]
compParser = sepBy1' oneComp (skipSpace >> (char ',') >> skipSpace)
  where
    oneComp = do
      a <- rational
      skipSpace >> (char ':') >> skipSpace
      b <- rational
      return $ SinComponent a b

randomSensors :: [SinComponent] -> [SinComponent] -> Rand (Double, Double, Double, Double)
randomSensors com spec = undefined

