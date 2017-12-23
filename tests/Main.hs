import Data.Geo.Swiss.Conversion

import Text.Printf
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Test
import System.Exit

main :: IO()
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

tests :: [(String, IO())]
tests = [ ("testRigi", runTest testRigi)
        , ("testBern", runTest testBern)
        , ("prop_ch95", runTest prop_ch95)
        , ("prop_ch95HP", runTest prop_ch95hp)]

runTest :: Testable prop => prop -> IO ()
runTest prop = do
  result <- quickCheckWithResult myArgs prop
  unless (isSuccess result) exitFailure
  return ()

-- | My default test arguments
myArgs :: Args
myArgs = Args
  { replay          = Nothing
  , maxSuccess      = 1000
  , maxDiscardRatio = 10
  , maxSize         = 1000
  , chatty          = True
  , maxShrinks      = 0
  }

-- | Make datatype CH95 eligible for testing with QuickCheck
instance Arbitrary CH95 where
  arbitrary =  do
       x <- choose(2480000, 2840000)
       y <- choose(1070000, 1300000)
       return $ LV95 x y

-- | Make datatype CH95HP eligible for testing with QuickCheck
instance Arbitrary CH95HP where
  arbitrary = do
       x <- choose(2480000, 2840000)
       y <- choose(1070000, 1300000)
       return $ LV95HP (round2digs x) (round2digs y)

{-
instance Arbitrary Degree where
  arbitrary =  do
    d <- choose (0, 90)
    m <- choose (0, 59)
    s <- choose (0, 59.99)
    return $ Deg d m (round2digs s)
-}

-- ----------------------------------------------------------------------------
-- Unit Tests
-- ----------------------------------------------------------------------------
testRigi :: Bool
testRigi = wgs2chHP wgsRigi == chRigi

testBern ::  Bool
testBern = wgs2ch wgsBern == chBern

-- ----------------------------------------------------------------------------
-- Property Tests
-- ----------------------------------------------------------------------------

-- | The easy ones: convert Swiss CH95 coordinates to world coordinates
--   and back again. Here we have a precision of 1 meter
prop_ch95 :: CH95 -> Bool
prop_ch95 x = (wgs2ch . ch2wgs) x == x

-- | The exacts: convert Swiss CH95 hight precision coordinates to world coordinates
--   and back again. Here we have a precision of 1 cm
prop_ch95hp :: CH95HP -> Bool
prop_ch95hp x = (wgs2chHP . ch2wgsHP) x == x

{-
-- | Property does not yet hold, not enough precision
prop_degree x = (rad2deg . deg2rad) x == x
   where types = x :: Degree
-}

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
