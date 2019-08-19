-- | Measurements related to magnetic properties of earth.
module Naqsha.Magnetic
       ( Variation(..)
       , Dip(..)
       ) where

import  Naqsha.Geometry.Angle

-- | The magnetic variation or declination
-- <https://en.wikipedia.org/wiki/Magnetic_declination Read on Wikipedia>.
newtype Variation = Variation Angle

-- | The magnetic dip or inclination <https://en.wikipedia.org/wiki/Magnetic_dip Read on Wikipedia>.
newtype Dip       = Dip       Angle
