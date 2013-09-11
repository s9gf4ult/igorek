{-# LANGUAGE
  ScopedTypeVariables
, BangPatterns
  #-}

module DTM.Helpers where


import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import DTM.Generator
import DTM.Parser
import DTM.Types
import Data.Attoparsec.Text.Lazy
import Data.List (foldl')
import Data.Ratio
import Data.Serialize (runGet, runPut)
import Data.Time
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as TL
import qualified Data.Vector.Unboxed as U

type SinComponent = (Double, Double) -- period, amplitude

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
    ms (Sensors a) = Sensors $ U.map f a

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


data InitialData = InitialData { iY :: (Double, Double, Double, Double)
                               , iZ :: (U.Vector Double, U.Vector Double, U.Vector Double, U.Vector Double)
                               , iHeader :: Header }


magicA, magicB :: Rational
magicA = 369 % 2500
magicB = (-37) % 1250

genX :: Double -> U.Vector Double -> U.Vector Double
genX y zs = U.map f zs
  where
    f z = fromRational $ (zz/magicA) - (yy * magicB/magicA)
      where
        zz = toRational z
        yy = toRational y

genData :: InitialData -> FullData
genData (InitialData
         (y1, y2, y3, y4)
         (z1, z2, z3, z4)
         hdr) = FullData
                hdr { hLength = (fromIntegral $ U.length sensors)*2 - 2
                    , hArg1 = round y1
                    , hArg2 = round y2
                    , hArg3 = round y3
                    , hArg4 = round y4
                    , hCommonArg = 5920 }
                $ Sensors sensors
  where
    sensors = U.zip4
              (U.map round $ genX y1 z1)
              (U.map round $ genX y2 z2)
              (U.map round $ genX y3 z3)
              (U.map round $ genX y4 z4)

parseComp :: TL.Text -> [CompRange]
parseComp t = case eitherResult $ parse compParser t of
  Left e -> error $ "sinus component must be in format: period:amplitude,period:amplitude... " ++ e
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
    digToComp (DRange a b) (DRange c d) = CompRange 1 (a, c) (b, d)
    digToComp (Digit a) (Digit b) = CompRepeat 1 (a, b)
    digToComp (Digit a) (DRange b c) = CompRange 1 (a, b) (a, c)
    digToComp (DRange a b) (Digit c) = CompRange 1 (a, c) (b, c)


randomSensors :: forall mr. (MonadRandom mr, Functor mr) => Int -> Double -> [CompRange] -> [CompRange] -> mr (U.Vector Double, U.Vector Double, U.Vector Double, U.Vector Double)
randomSensors len hi comR specR = do
  com <- (genSins . U.fromList . concat) <$> mapM rangeToComp comR
  spec1 <- (genSins . U.fromList . concat) <$> mapM rangeToComp specR
  spec2 <- (genSins . U.fromList . concat) <$> mapM rangeToComp specR
  spec3 <- (genSins . U.fromList . concat) <$> mapM rangeToComp specR
  spec4 <- (genSins . U.fromList . concat) <$> mapM rangeToComp specR
  return $ force (U.zipWith (\a b -> a + b + hi) com spec1,
                  U.zipWith (\a b -> a + b + hi) com spec2,
                  U.zipWith (\a b -> a + b + hi) com spec3,
                  U.zipWith (\a b -> a + b + hi) com spec4)
  where
    rangeToComp :: CompRange -> mr [(Double, SinComponent)]
    rangeToComp (CompRepeat i c@(per, _)) = replicateM i $ do
      ph <- getRandomR (0, per * 2)
      return (ph, c)
    rangeToComp (CompRange i (per1, amp1) (per2, amp2)) = replicateM i $ do
      per <- getRandomR (per1, per2)
      amp <- getRandomR (amp1, amp2)
      ph <- getRandomR (0, 2 * per)
      return (ph, (per, amp))

    genSins :: U.Vector (Double, SinComponent) -> U.Vector Double
    genSins comps = U.fromList $ map (genSin comps) [0,2..fromIntegral len]

    genSin :: U.Vector (Double, SinComponent) -> Double -> Double
    genSin comps x = U.foldl' (\res (ph, (per, amp)) -> res + (amp * (sin $ (x/per*2*pi)+ph ))) 0 comps


addGaps :: (MonadRandom mr, Functor mr) => (Int, Int) -> (Double, Double) -> FullData -> mr FullData
addGaps cc rr fd = do
  c <- getRandomR cc
  r <- (/100) <$> getRandomR rr
  let l = hLength $ fHeader fd
      gl :: Integer = round $ (fromIntegral l) * r
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
    mapSensors :: [(Int, Int)] -> SensorsData -> SensorsData
    mapSensors dps sns = foldl' foldVector sns dps
    foldVector vec (place, len) = U.imap (\i v -> if (i*2 >= place) && (i*2 <= place + len)
                                                  then (maxBound, maxBound, maxBound, maxBound)
                                                  else v) vec
