{-# LANGUAGE
  ScopedTypeVariables
  #-}

module DTM.Helpers where


import Control.DeepSeq.TH
import Control.DeepSeq
import Control.Monad.Random
import Control.Applicative
import Control.Monad
import DTM.Generator
import DTM.Parser
import DTM.Types
import Data.Serialize (runGet, runPut)
import Data.Time
import qualified Data.Vector as V
import Data.List (foldl')
import Data.Word
import Data.Ratio
import Data.Attoparsec.Text.Lazy
import Data.List (zip4, genericLength, foldl')
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

data SinComponent = SinComponent
                    { scPeriod :: Double
                    , scAmplitude :: Double }
                  deriving (Show, Eq)

data CompRange = CompRange
                 { crCount :: Int
                 , crStart :: SinComponent
                 , crEnd :: SinComponent
                 }
               | CompRepeat
                 { crCount :: Int
                 , crStart :: SinComponent
                 }
               deriving (Show, Eq)

data Digit a = Digit a
             | DRange a a
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

parseComp :: TL.Text -> [CompRange]
parseComp t = case eitherResult $ parse compParser t of
  Left e -> error "sinus component must be in format: period:amplitude,period:amplitude..."
  Right r  -> r

digitParser :: Parser a -> Parser (Digit a)
digitParser d = dRange <|> (Digit <$> d)
  where
    dRange = do
      a <- d
      (skipSpace >> char '-' >> skipSpace)
      b <- d
      return $ DRange a b

parseIntRange :: (Integral a) => TL.Text -> (a, a)
parseIntRange t = case eitherResult $ parse (digitParser decimal) t of
  Left e -> error $ "could not parse \"" ++ (TL.unpack t) ++ "\" as int number range" ++ e
  Right r -> case r of
    Digit d -> (d, d)
    DRange a b -> (a, b)

parseRatRange :: (Fractional a) => TL.Text -> (a, a)
parseRatRange t = case eitherResult $ parse (digitParser rational) t of
  Left e -> error $ "could not parse \"" ++ (TL.unpack t) ++ "\" as rational number range" ++ e
  Right r -> case r of
    Digit d -> (d, d)
    DRange a b -> (a, b)

compParser :: Parser [CompRange]
compParser = sepBy1' comp (skipSpace >> (char ',') >> skipSpace)
  where
    comp = manyComp <|> oneComp
    manyComp = do
      c <- oneComp
      (skipSpace >> char 'x' >> skipSpace)
      v <- decimal
      return $ c {crCount = v}
    oneComp = do
      a <- digitParser rational
      skipSpace >> (char ':') >> skipSpace
      b <- digitParser rational
      return $ digToComp a b
    digToComp (DRange a b) (DRange c d) = CompRange 1 (SinComponent a c) (SinComponent b d)
    digToComp (Digit a) (Digit b) = CompRepeat 1 $ SinComponent a b
    digToComp (Digit a) (DRange b c) = CompRange 1 (SinComponent a b) (SinComponent a c)
    digToComp (DRange a b) (Digit c) = CompRange 1 (SinComponent a c) (SinComponent b c)


randomSensors :: (MonadRandom mr, Functor mr) => Int -> Double -> [CompRange] -> [CompRange] -> mr ([Double], [Double], [Double], [Double])
randomSensors len hi comR specR = do
  com <- concat <$> mapM rangeToComp comR
  spec <- concat <$> mapM rangeToComp specR
  cphase <- mapM newPhase com
  let cdata = genSins $ zip cphase com
  phasea <- mapM newPhase spec
  phaseb <- mapM newPhase spec
  phasec <- mapM newPhase spec
  phased <- mapM newPhase spec
  return $ force (map (+hi) $ zipWith (+) cdata $ genSins $ zip phasea spec,
                  map (+hi) $ zipWith (+) cdata $ genSins $ zip phaseb spec,
                  map (+hi) $ zipWith (+) cdata $ genSins $ zip phasec spec,
                  map (+hi) $ zipWith (+) cdata $ genSins $ zip phased spec)
  where
    newPhase (SinComponent p _) = getRandomR (0, p*2)
    genSins p = foldl' (zipWith (+)) (replicate ((div len 2)+1) 0) $ map genSin p
    genSin (ph, (SinComponent per ampl)) = map (\x -> ampl * (sin ((x/per*2*pi) + ph))) [0,2..fromIntegral len]
    rangeToComp (CompRepeat i c) = return $ replicate i c
    rangeToComp (CompRange i (SinComponent a b) (SinComponent c d)) = replicateM i $ do
      x <- getRandomR (a, c)
      y <- getRandomR (b, d)
      return $ SinComponent x y


addGaps :: (MonadRandom mr, Functor mr) => (Int, Int) -> (Double, Double) -> FullData -> mr FullData
addGaps cc rr fd = do
  c <- getRandomR cc
  r <- (/100) <$> getRandomR rr
  let l = hLength $ fHeader fd
      gl = round $ (fromIntegral l) * r
      ga = gl `div` (fromIntegral c)

  ll <- replicateM c $ getRandomR (0, fromIntegral $ 2 * ga)
  case sum ll > fromIntegral gl of
    True -> addGaps cc rr fd
    False -> do
      places <- replicateM c $ getRandomR (0, fromIntegral l)
      let (Sensors ss) = fSensors fd
          news = mapSensors (zip places ll) ss
      return $ fd { fSensors = Sensors news }

  where

    mapSensors dps sns = V.toList $ foldl' foldVector (V.fromList sns) dps
    foldVector vec (place, len) = V.imap (\i v -> if (i*2 >= place) && (i*2 <= place + len)
                                                  then (maxBound, maxBound, maxBound, maxBound)
                                                  else v) vec
