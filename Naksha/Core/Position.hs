module Naksha.Core.Position
       ( -- * Latitude and longitudes
         Latitude, Longitude, lat, long
       -- ** Some common latitude
       , equator, northPole, southPole
         -- ** Some common longitude
       , greenwich
       -- * A geographic position.
       , Geo
       ) where

import Data.Int
import Data.Monoid
import Data.Group

newtype Latitude = Latitude Int64 deriving Eq

-- | Convert a real number to a latitude.
lat :: Double -> Latitude
lat = Latitude . normLat . floor . (*geoScale)

instance Show Latitude where
  show (Latitude x) = show int ++ "." ++ show frac
    where (int,frac) = x `quotRem` geoScale

instance Monoid Latitude where
  {-# INLINE mempty #-}
  mempty  = Latitude 0

  {-# INLINE mappend #-}
  mappend (Latitude x) (Latitude y) = Latitude $ normLat $ x + y

  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

instance Group Latitude where
  invert (Latitude x) = Latitude (-x)

-- | Longitude of a point
newtype Longitude = Longitude Int64

-- | Convert a real number to a longitude.
long :: Double -> Longitude
long = Longitude . normLong . floor . (*geoScale)

instance Eq Longitude where
  (==) (Longitude x) (Longitude y)
    | abs x == oneEighty = abs y == oneEighty -- 180° E = 180 = -180 = 180° W.
    | otherwise          = x == y

instance Show Longitude where
  show (Longitude x) = show int ++ "." ++ show frac
    where (int,frac) = x `quotRem` geoScale

instance Monoid Longitude where
  {-# INLINE mempty #-}
  mempty  = Longitude 0

  {-# INLINE mappend #-}
  mappend (Longitude x) (Longitude y) = Longitude $ normLong $ x + y

  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

instance Group Longitude where
  invert (Longitude x) = Longitude (-x)


-- | The latitude of equator.
equator :: Latitude
equator = lat 0

-- | The latitude of north pole.
northPole :: Latitude
northPole = lat 90

-- | The latitude of south pole.
southPole :: Latitude
southPole = lat (-90)


-- | The zero longitude.
greenwich :: Longitude
greenwich = long 0


-- | The coordinates of a point on the earth's surface.
data Geo = Geo {-# UNPACK #-} !Latitude
               {-# UNPACK #-} !Longitude

instance Eq Geo where
  (==) (Geo xlat xlong) (Geo ylat ylong)
    | xlat == northPole = ylat == northPole  -- longitude irrelevant for north pole
    | xlat == southPole = ylat == southPole  -- longitude irrelevant for south pole
    | otherwise         = xlat == ylat && xlong == ylong


--------------- Helper functions for latitude and longitudes.

-- | The scale of representation of latitude and longitude. Currently
-- it is number 10^7
geoScale :: Num a => a
geoScale = 10000000

ninety     :: Int64
ninety     = 90  * geoScale

oneEighty  :: Int64
oneEighty  = 180 * geoScale

twoSeventy :: Int64
twoSeventy = 270 * geoScale

threeSixty :: Int64
threeSixty = 360 * geoScale

-- | Function to normalise latitudes. It essentially is a saw-tooth
-- function of period 360 with max values 90.
normLat :: Int64 -> Int64
normLat y = signum y * normPosLat (abs y)

-- | Normalise a positive latitude.
normPosLat :: Int64 -> Int64
normPosLat pLat | r <= ninety     = r
                | r <= twoSeventy = oneEighty - r
                | otherwise       = r - threeSixty
  where r = pLat `rem` threeSixty

-- | Function to normalise longitude.
normLong :: Int64 -> Int64
normLong y = signum y * normPosLong (abs y)

-- | Normalise a positive longitude.
normPosLong :: Int64 -> Int64
normPosLong pLong | r <= oneEighty = r
                  | otherwise      = r - threeSixty
  where r   = pLong `rem` threeSixty

-- | Objects that have a location on the globe. Minimum complete
-- implementation either the two functions `longitude` and `latitude`
-- or the single function `geoPosition`.
class GeoLocation a where
  -- | The latitude of the object.
  latitude    :: a -> Latitude

  -- | The longitude of the object.
  longitude   :: a -> Longitude

  -- | The geo-Position of the object.
  geoPosition :: a -> Geo

  geoPosition a = Geo (latitude a) (longitude a)
  latitude      = latitude  . geoPosition
  longitude     = longitude . geoPosition


instance GeoLocation Geo where
  latitude  (Geo x _) = x
  longitude (Geo _ y) = y
  geoPosition         = id
