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

-- | Coordiantes in the WGS System:
--    first degrees lattitude, then degrees longitude
data WGS84 = WGS Degree Degree
instance Show WGS84 where
  show (WGS latt long)  = (show latt) ++ "; " ++ (show long) ++ ";"

-- | Coordinates in the current Swiss system "Landesvermessung 1995"
--   First longitude, then lattitude
data CH95 = LV95 Int Int
   deriving (Eq, Show)

-- | Coordinates in the old Swiss system "Landesvermessung 1903"
--   First longitude, then lattitude
data CH03 = LV03 Int Int
   deriving (Eq, Show)

-- | Convert World coordiantes to Swiss coordinates LV95
-- See : 3.2 Ellipsoid. Koordinaten (λ, φ) ⇒ Schweiz. Projektionskoordinaten (y, x)(strenge Formeln)
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
    yy' = round yy + 2600000 
    xx' = round xx + 1200000 

-- | Some examples. (Move later to tests)     
wgsBern :: WGS84
wgsBern = WGS (Deg 46 57 08.66) (Deg 7 26 22.50)

wgsRigi :: WGS84
wgsRigi = WGS (Deg 47 03 28.956559233) (Deg 8 29 11.11127154)

chBern :: CH95
chBern = LV95 2679520 12
