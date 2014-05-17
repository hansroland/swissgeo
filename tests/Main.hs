import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.Geo.Swiss.Conversion

{-
instance Arbitrary Degree where 
  arbitrary =  do
       d <- choose (0, 90)
       m <- choose (0, 59)
       s <- choose (0, 59.99)
       return $ Deg d m (round2digs s)
-}

-- | Make datatype CH95 eligible for testing with QuickCheck
instance Arbitrary CH95 where
  arbitrary =  do    
       x <- choose(2480000, 2840000) 
       y <- choose(1070000, 1300000)
       return $ LV95 x y

-- | 
instance Arbitrary CH95HP where
  arbitrary = do 
       x <- choose(2480000, 2840000) 
       y <- choose(1070000, 1300000)
       return $ LV95HP (round2digs x) (round2digs y)

main :: IO()
main = defaultMain tests

-- | All our tests: We have properties tests and unitTests
tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

-- | The property tests we do with 10000 test cases
properties :: TestTree
properties = localOption (QuickCheckTests 10000) $ testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "checked by QuickCheck"
           [testProperty "LV95" prop_ch95, 
            testProperty "LV95HP" prop_ch95hp]

{-
-- | Property does not yet hold, not enough precision
prop_degree x = (rad2deg . deg2rad) x == x 
   where types = x :: Degree
-}

-- | The easy ones: convert Swiss CH95 coordinates to world coordinates
--   and back again. Here we have a precision of 1 meter
prop_ch95 x = (wgs2ch . ch2wgs) x == x
   where types = x :: CH95 

-- | The exacts: convert Swiss CH95 hight precision coordinates to world coordinates
--   and back again. Here we have a precision of 1 cm
prop_ch95hp x = (wgs2chHP . ch2wgsHP) x == x
   where types = x :: CH95HP

unitTests = testGroup "Unit tests"
   [testCase "Rigi" $ wgs2chHP wgsRigi @?= chRigi,
    testCase "Bern" $ wgs2ch wgsBern @?= chBern]

-- ----------------------------------------------------------------------------
--  Some data for unit tests
-- ----------------------------------------------------------------------------
wgsBern :: WGS84
wgsBern = WGS (Deg 46 57 08.66) (Deg 7 26 22.50)

chBern :: CH95
chBern = LV95 2600000 1200000

wgsRigi :: WGS84
wgsRigi = WGS (Deg 47 03 28.956559233) (Deg 8 29 11.11127154)

chRigi :: CH95HP
chRigi = LV95HP 2679520.05 1212273.44
