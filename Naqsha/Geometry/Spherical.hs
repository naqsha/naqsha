-- | Geometric operations on earth surface assuming that earth is a
-- sphere of radius 6371008 m.
module Naqsha.Geometry.Spherical
       (
         -- * Distance calculation.
         distance, distance'
       , rMean
       ) where

import Data.Monoid
import Data.Group
import Naqsha.Geometry.Coordinate
import Naqsha.Geometry.Angle

--------------------- Distance calculation -------------------------------------

-- | Mean earth radius in meters. This is the radius used in the
-- haversine formula of `dHvs`.
rMean  :: Double
rMean = 6371008


-- | This combinator computes the distance (in meters) between two
-- geo-locations using the haversine distance between two points. For
-- `Position` which have an
distance :: Geo
         -> Geo
         -> Double -- ^ Distance in meters.
distance = distance' rMean


-- | A generalisation of `distance` that takes the radius as
-- argument. Will work on Mars for example once we set up a latitude
-- longitude system there. For this function units does not matter ---
-- the computed distance is in the same unit as the input radius. We
-- have
--
-- > distance = distance' rMean
--
distance' :: Double  -- ^ Radius (in whatever unit)
          -> Geo
          -> Geo
          -> Double

distance' r (Geo lat1 lon1) (Geo lat2 lon2) = r * c
  where p1    = toAngle lat1
        l1    = toAngle lon1
        p2    = toAngle lat2
        l2    = toAngle lon2
        dp    = p2 <> invert p1
        dl    = l2 <> invert l1

        phi1  = toRadian p1
        phi2  = toRadian p2

        a     = hav dp  + cos phi1 * cos phi2 * hav dl
        c     = 2 * atan2 (sqrt a) (sqrt (1 - a))

-- | The haversine functions.
hav :: Angle -> Double
{-# INLINE hav #-}
hav theta = stheta * stheta
  where stheta = sin (toRadian theta/2.0)
