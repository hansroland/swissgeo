-- | Geographical Degrees:  Deg degrees minutes seconds
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
-- data CH95 = LV95 Int Int
data CH95 = LV95 Double Double
   deriving (Eq, Show)

-- | Coordinates in the old Swiss system "Landesvermessung 1903"
--   First longitude, then lattitude
data CH03 = LV03 Int Int
   deriving (Eq, Show)

-- | Convert World coordiantes to Swiss coordinates LV95
-- Exact formula.
-- See : 3.2 Ellipsoid. Koordinaten (λ, φ) ⇒ Schweiz. Projektionskoordinaten (y, x)
wgs2ch :: WGS84 -> CH95
wgs2ch (WGS latt long) = LV95 yy' xx'
  where
    -- Input
    phi = deg2rad latt
    lam = deg2rad long
    -- Constants
    a = 6377397.155
    ee2 = 0.006674372230614
    ee = sqrt ee2
    phi0 = deg2rad (Deg 46 57 8.66)
    lam0 = deg2rad (Deg  7 26 22.5)
    rr = 6378815.90365
    alfa = 1.00072913843038
    b0 = deg2rad (Deg 46 54 27.83324844)
    kk = 0.0030667323772751
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
    yy' = {-round-} yy + 2600000
    xx' = {-round-} xx + 1200000

-- | Convert Swiss coordinates to World coordinates
-- Exact formula
-- See: 3.3 Schweizer Projektionskoordinaten (y, x) ⇒ ellipsoid. Koordinaten (λ, φ)
-- ch2wgs :: CH95 -> WGS
ch2wgs (LV95 y x) = WGS lam psi
  where
    -- Constants
    a = 6377397.155
    ee2 = 0.006674372230614
    ee = sqrt ee2
    phi0 = deg2rad (Deg 46 57 8.66)
    lam0 = deg2rad (Deg  7 26 22.5)
    rr = 6378815.90365
    alfa = 1.00072913843038
    b0 = deg2rad (Deg 46 54 27.83324844)
    kk = 0.0030667323772751
    -- a) Projektionsebene (y, x) ⇒ Kugel ( b , l )
    yy = {-fromIntegral-} y - 2600000
    xx = {-fromIntegral-} x - 1200000
    l' = yy / rr
    b' = 2 * atan (exp (xx/rr)) - pi/2    -- pi/2 instead of pi/4 outside 2*
    -- b) Pseudoäquatorsystem ( b , l ) ⇒ Äquatorsystem (b, l)
    b = asin(cos b0 * sin b' + sin b0 * cos b' * cos l')
    l = atan (sin l') / (cos b0 * cos l' - sin b0 * tan b')
    -- c) Kugel (b, l) ⇒ Ellipsoid (φ, λ)
    lam = rad2deg $ lam0 + l/alfa
    -- The base functions for the iteration
    fss psi = 1 / alfa * (log (tan (pi/4 + b/2)) - kk) + ee
      * log (tan (pi/4 + asin(ee * sin psi) / 2))
    fpsi ss = 2 * atan (exp ss) - pi/2
    -- To avoid infinite loops, do at most 50 iterations,
    -- hopefully it stops earlier ...
    -- Normally we need 5 to 7 iterations.
    psi = rad2deg $ firstEqual $ take 50 $ iterate (fpsi . fss) b


-- | Find the convergence of a sequence.
-- We just search for the first element that is euql to its successsor
firstEqual :: [Double] -> Double
firstEqual [] = 0                  -- this should not occur
firstEqual [x] = x
firstEqual (x : y : zs)
    | x == y = x
    | otherwise = firstEqual (y : zs)


-- | Some examples. (Move later to tests)
wgsBern :: WGS84
wgsBern = WGS (Deg 46 57 08.66) (Deg 7 26 22.50)

wgsRigi :: WGS84
wgsRigi = WGS (Deg 47 03 28.956559233) (Deg 8 29 11.11127154)

chRigi :: CH95
chRigi = LV95 2679520.05 1212273.44
