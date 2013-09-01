{-# LANGUAGE
  TemplateHaskell
  #-}

module Main where


import Control.Applicative
import DTM.Generator
import DTM.Parser
import DTM.Types
import Data.Derive.Arbitrary
import Data.DeriveTH
import Data.Serialize
import Data.Word
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Runners.Console
import Test.QuickCheck
import Test.QuickCheck.Assertions
import Test.QuickCheck.Instances ()
import qualified Data.Text.Lazy as TL
import qualified Test.QuickCheck.Property as P

arbSafeText :: Gen TL.Text
arbSafeText = TL.pack <$> ((flip suchThat) ((<32) . length) $ listOf $ elements
                           $ ['a'..'z'] ++ ['A'..'Z']
                           ++ ['а'..'я'] ++ ['А'..'Я']
                           ++ ['0'..'9'])

instance Arbitrary Header where
  arbitrary = Header
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbSafeText
              <*> arbSafeText
              <*> arbitrary
              <*> (((/10) . fromIntegral) <$> (arbitrary :: Gen Word16))
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary

$( derive makeArbitrary ''DateTime )
instance Arbitrary Sensors where
  arbitrary = Sensors <$> arbitrary
$( derive makeArbitrary ''FullData )

cmpFD :: FullData -> FullData -> Property
cmpFD (FullData a b) (FullData aa bb) = (cmpHD a aa) P..&&. (b ==? bb)

cmpHD hd1 hd2 = P.conjoin
                [ cm hMagicWords
                , cm hStartDate
                , cm hEndDate
                , cm hPlot 
                , cm hProduct 
                , cm hDiameter 
                , cm hThickness
                , cm hBlock
                , cm hTemperature
                , cm hLength 
                , cm hProdNumber 
                , cm hArg1 
                , cm hArg2 
                , cm hArg3 
                , cm hArg4 
                , cm hCommonArg
                ]
  where
    cm :: (Eq a, Show a) => (Header -> a) -> P.Result
    cm f = (f hd1) ==? (f hd2)

-- checkGen :: FullData -> Property
checkGen fd = case runGet parseFullData $ runPut $ genFullData fd of
  Left e -> P.once $ P.failed {P.reason = "parser failed with: " ++ e}
  Right res -> cmpFD res fd
  

main :: IO ()
main = defaultMain [testProperty "Generator work with parser" checkGen]
