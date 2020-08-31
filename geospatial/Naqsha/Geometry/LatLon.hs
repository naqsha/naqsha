{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE Rank2Types                 #-}
-- | This module captures position of a point on the globe.
module Naqsha.Geometry.LatLon
       ( -- * Latitude longitudes and Points on Globle
         -- $latandlong$
         module Naqsha.Geometry.LatLon.Internal
       ) where


import Data.Group                      ( invert )
import Naqsha.Geometry.Angle
