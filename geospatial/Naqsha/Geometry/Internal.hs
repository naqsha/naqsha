{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

-- | The internal module that exposes the basic geometric types in
-- naqsha. This interface is subject to change and hence use with
-- caution.
module Naqsha.Geometry.Internal
  ( module Naqsha.Geometry.Angle.Internal
  , module Naqsha.Geometry.LatLon.Internal
  ) where

import Naqsha.Geometry.Angle.Internal        ( Angle )
import Naqsha.Geometry.Angle.Internal hiding ( Angle )
import Naqsha.Geometry.LatLon.Internal (Latitude, Longitude, LatLon)
import Naqsha.Geometry.LatLon.Internal hiding (Latitude, Longitude, LatLon)

