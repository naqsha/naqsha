{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE Rank2Types                 #-}
-- | This module captures position of a point on the globe.
module Naqsha.Geometry.Coordinate
       ( -- * Basics
         -- $latandlong$
         Geo(..)
       , northPole, southPole
       -- ** Latitudes
       , Latitude
       , lat, north, south
       , equator
       , tropicOfCancer
       , tropicOfCapricon
       -- ** Longitudes.
       , Longitude
       , lon, east, west
       , greenwich
       ) where

import           Control.Monad               ( liftM )
import           Data.Group
import           Data.Vector.Unboxed         ( MVector(..), Vector)
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GVM


import           Prelude         -- To avoid redundunt import warnings.

import           Naqsha.Geometry.Angle
import           Naqsha.Geometry.Internal



-- $latandlong$
--
-- A point on the globe is specified by giving its geo coordinates
-- represented by the type `Geo`.  It is essentially a pair of the
-- `Latitude` and `Longitude` of the point.
--
-- == Examples
--
-- > kanpurLatitude  :: Latitude
-- > kanpurLatitude  = lat $ degree 26.4477777
-- > kanpurLongitude :: Longitude
-- > kanpurLongitude = lon $ degree 80.3461111
-- > kanpurGeo       :: Geo
-- > kanpurGeo       = Geo kanpurLatitude kanpurLongitude
--
-- You can also specify the latitude and longitude in units of degree,
-- minute and seconds.
--
-- > kanpurLatitude  = lat $ degree 26 <> minute 26 <> second 52
-- > kanpurLongitude = lon $ degree 80 <> minute 20 <> second 46
--
-- The show and read instance of the `Latitude` and `Longitude` types
-- uses degrees for displaying and reading respectively. Show and Read
-- instances can express these quantities up to Nano degree precision.
--
-- == Convention on sign.
--
-- For latitudes, positive means north of the equator and negative
-- means south. In the case of longitudes, positive means east of the
-- longitude zero and negative means west. However, if you find these
-- conventions confusing you can use the combinators `north`, `south`,
-- `east`, and `west` when constructing latitudes or longitudes.
--


-- | Convert an angle to a northern latitude
--
-- > tropicOfCancer = north $ degree 23.5
--
north :: Angle -> Latitude
north = lat

-- | Convert an angle to a southern latitude.
--
-- >  tropicOfCapricon = south $ degree 23.5
--
south :: Angle -> Latitude
south = lat . invert


-- | The latitude of equator.
equator :: Latitude
equator = lat $ degree 0

-- | The latitude corresponding to the Tropic of Cancer.
tropicOfCancer :: Latitude
tropicOfCancer = north $ degree 23.5

-- | The latitude corresponding to the Tropic of Capricon
tropicOfCapricon :: Latitude
tropicOfCapricon = south $ degree 23.5

-- | Convert angle to an eastern longitude.
--
-- > kanpurLongitude = east $ degree 80.3461
--
east :: Angle -> Longitude
east = lon

-- | Convert angle to a western longitude
--
-- > newyorkLongitude = west $ degree 74.0059
--
west :: Angle -> Longitude
west = lon . invert


-- | The zero longitude.
greenwich :: Longitude
greenwich = lon $ degree 0


------------------- The geometric coordinates. -----------------

-- | The coordinates of a point on the earth's surface.
data Geo = Geo {-# UNPACK #-} !Latitude
               {-# UNPACK #-} !Longitude
         deriving Show


-- | The North pole
northPole :: Geo
northPole = Geo maxBound $ lon $ degree 0

-- | The South pole
southPole :: Geo
southPole = Geo minBound $ lon $ degree 0

instance Eq Geo where
  (==) (Geo xlat xlong) (Geo ylat ylong)
    | xlat == maxBound = ylat == maxBound  -- longitude irrelevant for north pole
    | xlat == minBound = ylat == minBound  -- longitude irrelevant for south pole
    | otherwise     = xlat == ylat && xlong == ylong

----------------------------- Vector Instance for Geo ---------------------------------------------


newtype instance MVector s Geo = MGeoV (MVector s (Angle,Angle))
newtype instance Vector    Geo = GeoV  (Vector    (Angle,Angle))


instance GVM.MVector MVector Geo where
  {-# INLINE basicLength          #-}
  {-# INLINE basicUnsafeSlice     #-}
  {-# INLINE basicOverlaps        #-}
  {-# INLINE basicUnsafeNew       #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead      #-}
  {-# INLINE basicUnsafeWrite     #-}
  {-# INLINE basicClear           #-}
  {-# INLINE basicSet             #-}
  {-# INLINE basicUnsafeCopy      #-}
  {-# INLINE basicUnsafeGrow      #-}
  basicLength          (MGeoV v)         = GVM.basicLength v
  basicUnsafeSlice i n (MGeoV v)         = MGeoV $ GVM.basicUnsafeSlice i n v
  basicOverlaps (MGeoV v1) (MGeoV v2)    = GVM.basicOverlaps v1 v2

  basicUnsafeRead  (MGeoV v) i           = do (x,y) <- GVM.basicUnsafeRead v i
                                              return $ Geo (Latitude x) $ Longitude y
  basicUnsafeWrite (MGeoV v) i (Geo x y) = GVM.basicUnsafeWrite v i (unLat x, unLong y)

  basicClear (MGeoV v)                   = GVM.basicClear v
  basicSet   (MGeoV v)         (Geo x y) = GVM.basicSet v (unLat x, unLong y)

  basicUnsafeNew n                       = MGeoV `liftM` GVM.basicUnsafeNew n
  basicUnsafeReplicate n     (Geo x y)   = MGeoV `liftM` GVM.basicUnsafeReplicate n (unLat x, unLong y)
  basicUnsafeCopy (MGeoV v1) (MGeoV v2)  = GVM.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MGeoV v)   n          = MGeoV `liftM` GVM.basicUnsafeGrow v n

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MGeoV v)              = GVM.basicInitialize v
#endif

instance GV.Vector Vector Geo where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MGeoV v)         = GeoV  `liftM` GV.basicUnsafeFreeze v
  basicUnsafeThaw (GeoV v)            = MGeoV `liftM` GV.basicUnsafeThaw v
  basicLength (GeoV v)                = GV.basicLength v
  basicUnsafeSlice i n (GeoV v)       = GeoV $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM (GeoV v) i        =do (x,y) <- GV.basicUnsafeIndexM v i
                                          return $ Geo (Latitude x) $ Longitude y

  basicUnsafeCopy (MGeoV mv) (GeoV v) = GV.basicUnsafeCopy mv v
  elemseq _ (Geo x y)                 = GV.elemseq (undefined :: Vector a) (unLat x, unLong y)
