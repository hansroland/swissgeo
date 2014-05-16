{-# LANGUAGE NamedFieldPuns #-}

module Data.Geo.Swiss.Conversion
  where

-- | Geographical Degrees:  Deg degrees minutes seconds
--   Example: Deg 46 57 8.66 
data Degree = Deg Int Int Double
   deriving (Eq, Show)

-- | Convert Degrees to Radians
deg2rad :: Degree -> Double
deg2rad (Deg d m s) = deg * pi /180
  where
    deg = d' + m'/60 + s/3600
    d' = fromIntegral d
    m' = fromIntegral m

rad2deg :: Double -> Degree
rad2deg rad = Deg d m s
   where
     dd = 180 * rad / pi
     d = floor dd
     mm = (dd - fromIntegral d) * 60
     m = floor mm
     s = (mm - fromIntegral m) * 60

-- | Coordiantes in the WGS System:
--    first degrees lattitude, then degrees longitude
data WGS84 = WGS Degree Degree
instance Show WGS84 where
  show (WGS latt long)  = show latt ++ "; " ++ show long ++ ";"

-- | Coordinates in the current Swiss system "Landesvermessung 1995"
--   First longitude, then lattitude
--   Here accuracy is 1 meter.
--   Note: Your GPS measurements may NOT have this precision!!
data CH95 = LV95 Int Int
   deriving (Eq, Show)

-- | High precision version of Swiss system "Landesvermessung 1995"
-- Here accuracy is better than 1 cm.
-- We normally round to 2 decimal digits (= 1 cm).
-- Note: Your GPS measurements may NOT have this precision!!
data CH95HP = LV95HP Double Double
   deriving (Eq, Show)

-- | Coordinates in the old Swiss system "Landesvermessung 1903"
--   First longitude, then lattitude
--   Here accuracy is 1 meter.
data CH03 = LV03 Int Int
   deriving (Eq, Show)

-- | Conversion from the High Precision System to the normal version
fromHP :: CH95HP -> CH95
fromHP (LV95HP y x) = LV95 (round y) (round x)

-- | Conversion fron normal 1-meter coordiantes to 1 cm coordinates.
-- Note: We cannot add the missing accuracy in centimeters!
toHP :: CH95 -> CH95HP
toHP (LV95 y x) = LV95HP (fromIntegral y) (fromIntegral x)

-- | Constants used in both conversion functions
data Consts = Consts {
   ee  :: Double                -- ee**2 = 1.numerische Exzentrizität (im Quadrat) des Bessel-Ellipsoids
   , lam0 :: Double             -- geogr. Länge des Nullpunkts in Bern
   , rr :: Double               -- Radius der Projektionskugel
   , alfa :: Double             -- Verhältnis Kugellänge zu Ellipsoidlänge
   , b0 :: Double               -- Breite des Nullpunkts auf der Kugel
   , kk :: Double               -- Konstante der Breitenformel
   }

-- | Constants used in the calculations
consts :: Consts
consts = Consts {
   ee = sqrt 0.006674372230614
   , lam0 = deg2rad (Deg  7 26 22.5)
   , rr = 6378815.90365
   , alfa = 1.00072913843038
   , b0 = deg2rad (Deg 46 54 27.83324844)
   , kk = 0.0030667323772751
   }

-- | Convert World coordiantes to Swiss coordinates LV95
-- Exact formula.
-- See : 3.2 Ellipsoid. Koordinaten (λ, φ) ⇒ Schweiz. Projektionskoordinaten (y, x)
wgs2chHP :: WGS84 -> CH95HP
wgs2chHP (WGS latt long) = LV95HP yy' xx'
  where
    -- Input
    phi = deg2rad latt
    lam = deg2rad long
    -- Constants
    Consts {ee, lam0, rr, alfa, b0, kk} = consts
    -- a) Ellipsoid (φ, λ) ⇒ Kugel (b, l) (Gauss'sche Projektion)
    ss = alfa * log(tan(pi/4 + phi/2) ) - alfa * ee/2
       * log ((1 + ee * sin phi) / (1 - ee * sin phi)) + kk
    b = 2 * (atan (exp ss) - pi/4)
    l = alfa * (lam -lam0)
    -- b) Äquatorsystem (b, l) ⇒ Pseudoäquatorsystem ( b , l ) (Rotation)
    l' = atan (sin l / (sin b0 * tan b + cos b0  * cos l) )
    b' = asin (cos b0 * sin b - sin b0 * cos b * cos l)
    -- c) Kugel ( b , l ) ⇒ Projektionsebene (y, x) (Mercator-Projektion)
    yy = rr * l'
    xx = rr/2 * log ((1 + sin b') / (1 - sin b'))
    yy' = round2dec yy + 2600000
    xx' = round2dec xx + 1200000

-- | Convert Word coordinates to Swiss coordinates
-- Accuracy is 1 meter
wgs2ch :: WGS84 -> CH95
wgs2ch = fromHP . wgs2chHP

-- | Convert Swiss coordinates to World coordinates
-- Exact formula
-- See: 3.3 Schweizer Projektionskoordinaten (y, x) ⇒ ellipsoid. Koordinaten (λ, φ)
ch2wgsHP :: CH95HP -> WGS84
ch2wgsHP (LV95HP y x) = WGS psi lam
  where
    -- Constants
    Consts {ee, lam0, rr, alfa, b0, kk} = consts
    -- a) Projektionsebene (y, x) ⇒ Kugel ( b , l )
    yy = y - 2600000
    xx = x - 1200000
    l' = yy / rr
    b' = 2 * atan (exp (xx/rr)) - pi/2    -- pi/2 instead of pi/4 outside 2*
    -- b) Pseudoäquatorsystem ( b , l ) ⇒ Äquatorsystem (b, l)
    b = asin(cos b0 * sin b' + sin b0 * cos b' * cos l')
    l = atan $ sin l' / (cos b0 * cos l' - sin b0 * tan b')
    -- c) Kugel (b, l) ⇒ Ellipsoid (φ, λ)
    lam = rad2deg $ lam0 + l/alfa
    -- The base functions for the iteration
    fss psx = 1 / alfa * (log (tan (pi/4 + b/2)) - kk) + ee
      * log (tan (pi/4 + asin(ee * sin psx) / 2))
    fpsi ss = 2 * atan (exp ss) - pi/2
    -- To avoid infinite loops, do at most 50 iterations,
    -- hopefully it stops earlier ...
    -- Normally we need 5 to 7 iterations.
    psi = rad2deg $ firstEqual $ take 50 $ iterate (fpsi . fss) b

-- | Convert Swiss coordinates to World coordinates (1 meter accuracy)
ch2wgs :: CH95 -> WGS84
ch2wgs = ch2wgsHP . toHP

-- | Find the convergence of a sequence.
-- We just search for the first element that is euql to its successsor
firstEqual :: [Double] -> Double
firstEqual [] = 0                  -- this should not occur
firstEqual [x] = x
firstEqual (x : y : zs)
    | x == y = x
    | otherwise = firstEqual (y : zs)

-- | round a Double to 2 digits after the decimal point
round2dec :: Double -> Double
round2dec d = 
    let i = (round (d * 100)) :: Int
    in fromIntegral i / 100

-- | Some examples. (Move later to tests)
wgsBern :: WGS84
wgsBern = WGS (Deg 46 57 08.66) (Deg 7 26 22.50)

chBern :: CH95
chBern = LV95 2600000 1200000

wgsRigi :: WGS84
wgsRigi = WGS (Deg 47 03 28.956559233) (Deg 8 29 11.11127154)

chRigi :: CH95HP
chRigi = LV95HP 2679520.05 1212273.44
