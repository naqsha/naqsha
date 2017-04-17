-- | Geometric operations on earth surface assuming that earth is a
-- sphere of radius 6371008 m.
module Naqsha.Geometry.Spherical
       (
         -- * Distance calculation.
         distance, distance'
       , rMean
       ) where

import Control.Lens
import Naqsha.Position

--------------------- Distance calculation -------------------------------------

-- | Mean earth radius in meters. This is the radius used in the
-- haversine formula of `dHvs`.
rMean  :: Double
rMean = 6371008


-- | This combinator computes the distance (in meters) between two
-- geo-locations using the haversine distance between two points. For
-- `Position` which have an
distance :: ( Location geo1
            , Location geo2
            )
         => geo1   -- ^ Point 1
         -> geo2   -- ^ Point 2
         -> Double -- ^ Distance in meters.
distance = distance' rMean

{-# SPECIALISE distance :: Geo      -> Geo      -> Double #-}

-- | A generalisation of `dHvS` that takes the radius as
-- argument. Will work on Mars for example once we set up a latitude
-- longitude system there. For this function units does not matter ---
-- the computed distance is in the same unit as the input radius. We
-- have
--
-- > distance = distance' rMean
--
distance' :: ( Location geo1
             , Location geo2
             )
          => Double  -- ^ Radius (in whatever unit)
          -> geo1     -- ^ Point 1
          -> geo2     -- ^ Point 2
          -> Double
{-# SPECIALISE distance' :: Double -> Geo      -> Geo      -> Double #-}
distance' r g1 g2 = r * c
  where p1    = toRad $ g1 ^. latitude
        l1    = toRad $ g1 ^. longitude
        p2    = toRad $ g2 ^. latitude
        l2    = toRad $ g2 ^. longitude
        dp    = p2 - p1
        dl    = l2 - l1
        a     = hav dp  + cos p1 * cos p2 * hav dl
        c     = 2 * atan2 (sqrt a) (sqrt (1 - a))

-- | The haversine functions.
hav :: Double -> Double
{-# INLINE hav #-}
hav theta = stheta * stheta
  where stheta = sin (theta/2.0)
