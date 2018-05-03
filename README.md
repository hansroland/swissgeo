### Convert World Coordinates to Swiss Coordinates

[![License BSD3][license-badge]][license]
[![Build Status][build-badge]][build-result]

[license-badge]: https://img.shields.io/badge/license-BSD3-green.svg?dummy
[license]: https://github.com/hansroland/swissgeo/blob/master/LICENSE

[build-badge]: https://travis-ci.org/hansroland/swissgeo.svg?branch=master
[build-result]: https://travis-ci.org/hansroland/swissgeo


World Coordinates are the output of a normal GPS device.

Swiss Coordinates or "Landeskoordinaten" are the coordinates on the official Swiss maps of swisstopo.

The city of Bern is 46 degress 57 minutes and 08.66 seconds North and 7 degrees 26 minutes and 22.50 seconds East. That's what a GPS measures.

In terms of Swiss "Landeskoordinaten 1903" this is 600000 / 200000.

In terms of the new "Landeskoordinaten 1995" this is 2600000 / 1200000.

This little library contains Haskell data types and functions to convert
from wold coordinates to swiss coordinates and back.

For the conversion formulas see:

#### Reference:

* [Formeln und Konstanten für die Berechnung der
Schweizerischen schiefachsigen Zylinderprojektion und
der Transformation zwischen Koordinatensystemen](https://github.com/hansroland/swissgeo/blob/master/doc/refsysd.pdf)

swissgeo implements the exact formulas.

#### Data types:

    data Degree = Deg Int Int Double    -- degrees
    data WGS84  = WGS Degree Degree     -- world coordinates (lattitude longitude)
    data CH03   = LV03 Int Int          -- Swiss coordinates system 1903
    data CH95   = LV95 Int Int          -- Swiss coordinates system 1995 (precision 1 meter)
    data CH95HP = LV95HP Double Double  -- Swiss coordinates system 1995 (precision 1 cm)

#### Functions

    wgs2ch :: WGS84 -> CH95             -- Convert world to Swiss coordinates 1995 (1 meter)
    wgs2chHP :: WGS84 -> CH95HP         -- Convert world to Swiss coordinates 1995 (1 cm)
    ch2wgs :: CH95 -> WGS84             -- Convert Swiss  coordinates to world. (1 meter)
    ch2wgsHP :: CH95HP -> WGS84         -- Convert Swiss  coordinates to world. (1 cm)

    to95 :: CH03 -> CH95                -- Convert from 1903 to 1995 Swiss coordinates
    to03 :: CH95 -> CH03                -- Convert from 1995 to 1903 Swiss coordinates