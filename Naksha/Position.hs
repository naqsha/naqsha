{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

-- | This module captures position of a point on the globe.
module Naksha.Position
       ( -- * Latitude and longitude and geopositions.
         -- $latandlong$
         Latitude, Longitude, Geo(..)
       , lat, long, minute, second,
       -- ** Some common latitude
       , equator, northPole, southPole
         -- ** Some common longitude
       , greenwich
       -- ** Objects with elevation
       , Elevated, elevate, altitude, Position
       , MaybeElevated(..)
       -- * A geographic position.
       , Location(..)

       -- * Distance calculation.
       , dHvS, dHvS'
       ) where

import           Control.Monad               ( liftM )
import           Data.Group
import           Data.Int
import           Data.Monoid
import           Data.Vector.Unboxed         ( MVector(..), Vector, Unbox)
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GVM

-- $latandlong$
--
-- A point on the globe is specified by giving its geo coordinates
-- captures by the type `Geo`.  It is essentially a pair of the
-- `Latitude` and `Longitude` of the point.
--
-- The types `Latitude` and `Longitude` are opaque types and they are
-- constructed using the smart constructors `lat` and `long`
-- respectively.
--
-- > kanpurLatitude :: Latitude
-- > kanpurLatitude  = lat 26.4477777
-- > kanpurLongitude :: Longitude
-- > kanpurLongitude = long 80.3461111
--
-- If one wants to express it in minutes and seconds it is a bit more
-- involved.
--
-- > kanpurLatitude = lat  $ 26 + 26 * minute + 52 * second
-- > kanpurLatitude = long $ 80 + 20 * minute + 46 * second
--
-- Finally there is the `Location` class which captures elements
-- that have a valid geo location.
--


----------------------------- Lattitude ----------------------------------

-- | The latitude of a point.
newtype Latitude = Latitude { unLat :: Double } deriving Show

-- | Create a latitude from double.
lat :: Double -> Latitude
lat  = Latitude

-- | Normalise latitude in the range (-90,90)
normaliseLat :: Latitude -> Latitude
normaliseLat = lat . normLat . unLat

instance Eq Latitude where
  (==) l1 l2 = unLat (normaliseLat l1) == unLat (normaliseLat l2)

-- | The latitude of equator.
equator :: Latitude
equator = lat 0

-- | The latitude of north pole.
northPole :: Latitude
northPole = lat 90

-- | The latitude of south pole.
southPole :: Latitude
southPole = lat (-90)

-------------------------- Longitude ------------------------------------------

-- | The longitude of a point
newtype Longitude = Longitude { unLong :: Double } deriving Show

-- | Create longitude from a double.
long :: Double -> Longitude
long = Longitude

-- | Normalise longitude to the range (-180, 180).
normaliseLong :: Longitude -> Longitude
normaliseLong = long . normLong . unLong

instance Eq Longitude  where
  (==) l1 l2 = unLong (normaliseLong l1) == unLong (normaliseLong l2)

-- | The zero longitude.
greenwich :: Longitude
greenwich = long 0


-- | The coordinates of a point on the earth's surface.
data Geo = Geo {-# UNPACK #-} !Latitude
               {-# UNPACK #-} !Longitude


-- | Objects that have a location on the globe. Minimum complete
-- implementation either the two functions `longitude` and `latitude`
-- or the single function `geoPosition`.
class Location a where
  -- | The latitude of the object.
  latitude    :: a -> Latitude

  -- | The longitude of the object.
  longitude   :: a -> Longitude

  -- | The geo-Position of the object.
  geoPosition :: a -> Geo

  geoPosition a = Geo (latitude a) (longitude a)
  latitude      = latitude  . geoPosition
  longitude     = longitude . geoPosition


instance Location Geo where
  latitude  (Geo x _) = x
  longitude (Geo _ y) = y
  geoPosition         = id



-- | An object with a possible elevation, i.e. that comes with an elevation.
data Elevated a = Elevated { unElevate   :: a
                           , altitude    :: Double
                           }

-- | A point is a Geo location together with an elevation.
type Position = Elevated Geo

-- | Attach an elevation to an object.
elevate  :: a            -- ^ Stuff to which an elevation has to be attached.
         -> Double       -- ^ Elevation in meters from mean-sealevel
         -> Elevated a
elevate  = Elevated


instance Location a => Location (Elevated a) where
  latitude     = latitude    . unElevate
  longitude    = longitude   . unElevate
  geoPosition  = geoPosition . unElevate

class Location a => MaybeElevated a where
  getAltitude :: a -> Maybe Double
  getPosition :: a -> Maybe Position

instance Eq Geo where
  (==) (Geo xlat xlong) (Geo ylat ylong)
    | xlat == northPole = ylat == northPole  -- longitude irrelevant for north pole
    | xlat == southPole = ylat == southPole  -- longitude irrelevant for south pole
    | otherwise         = xlat == ylat && xlong == ylong

--------------------- Distance calculation -------------------------------------

-- | Mean earth radius in meters.
rMean  :: Double
rMean = 637100.88


-- | This combinator computes the distance between two geo-locations
-- using the haversine distance between two points.
dHvS :: Geo -> Geo -> Double
dHvS = dHvS' rMean

-- | A generalisation of dHaverSine that takes the radius as argument.
dHvS' :: Double -> Geo -> Geo -> Double
dHvS' r g1 g2 = r * c
  where p1    = rad $ unLat  $ latitude  g1
        l1    = rad $ unLong $ longitude g1
        p2    = rad $ unLat  $ latitude  g2
        l2    = rad $ unLong $ longitude g2
        dp    = p2 - p1
        dl    = l2 - l1
        a     = (sin $ dp/2.0)^(2 :: Int) + cos p1 * cos p2 * ((sin $ dl/2)^(2 :: Int))
        c     = 2 * atan2 (sqrt a) (sqrt (1 - a))


-- | Convert to radians
rad  :: Double -> Double
rad x = (pi * x) /180

--------------- Helper functions for latitude and longitudes.


-- | Angle of 1 minute in decimals. You can use `42 * minute` for
-- expressing an angle of 42 minutes.
minute  :: Double
minute = 1/60

-- | Angle of 1 second in decimals. You can use `42 * second` for
-- expressing an angle of 42 seconds.
second :: Double
second = (1/3600)

--------------------------- Internal helper functions ------------------------


-- | Function to normalise latitudes. It essentially is a saw-tooth
-- function of period 360 with max values 90.
normLat :: Double -> Double
normLat y = signum y * normPosLat (abs y)

-- | Normalise a positive latitude.
normPosLat :: Double -> Double
normPosLat pLat | r <= 90    = r
                | r <= 270   = 180 - r
                | otherwise  = r -  360
  where f = snd (properFraction $ pLat / 360 :: (Int, Double))
        r  = f * 360

-- | Function to normalise longitude.
normLong :: Double -> Double
normLong y = signum y * normPosLong (abs y)

-- | Normalise a positive longitude.
normPosLong :: Double -> Double
normPosLong pLong | r <= 180  = r
                  | otherwise = r - 360
  where f  = snd (properFraction $ pLong / 360 :: (Int, Double))
        r  = f * 360


------------------- Making stuff suitable for unboxed vector. --------------------------

newtype instance MVector s Latitude = MLatV (MVector s Double)
newtype instance Vector    Latitude = LatV  (Vector Double)


newtype instance MVector s Longitude = MLongV (MVector s Double)
newtype instance Vector    Longitude = LongV  (Vector Double)


newtype instance MVector s Geo = MGeoV (MVector s (Double,Double))
newtype instance Vector    Geo = GeoV  (Vector    (Double,Double))


-------------------- Instance for latitude --------------------------------------------

instance GVM.MVector MVector Latitude where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength          (MLatV v)              = GVM.basicLength v
  basicUnsafeSlice i n (MLatV v)              = MLatV $ GVM.basicUnsafeSlice i n v
  basicOverlaps (MLatV v1) (MLatV v2)         = GVM.basicOverlaps v1 v2

  basicUnsafeRead  (MLatV v) i                = Latitude `liftM` GVM.basicUnsafeRead v i
  basicUnsafeWrite (MLatV v) i (Latitude x)   = GVM.basicUnsafeWrite v i x

  basicClear (MLatV v)                        = GVM.basicClear v
  basicSet   (MLatV v)         (Latitude x)   = GVM.basicSet v x

  basicUnsafeNew n                            = MLatV `liftM` GVM.basicUnsafeNew n
  basicUnsafeReplicate n     (Latitude x)     = MLatV `liftM` GVM.basicUnsafeReplicate n x
  basicUnsafeCopy (MLatV v1) (MLatV v2)       = GVM.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MLatV v)   n               = MLatV `liftM` GVM.basicUnsafeGrow v n

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MLatV v)                   = GVM.basicInitialize v
#endif

instance GV.Vector Vector Latitude where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MLatV v)         = LatV  `liftM` GV.basicUnsafeFreeze v
  basicUnsafeThaw (LatV v)            = MLatV `liftM` GV.basicUnsafeThaw v
  basicLength (LatV v)                = GV.basicLength v
  basicUnsafeSlice i n (LatV v)       = LatV $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM (LatV v) i        = Latitude   `liftM`  GV.basicUnsafeIndexM v i

  basicUnsafeCopy (MLatV mv) (LatV v) = GV.basicUnsafeCopy mv v
  elemseq _ (Latitude x)              = GV.elemseq (undefined :: Vector a) x


-------------------------------- Instance for Longitude -----------------------------------

instance GVM.MVector MVector Longitude where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength          (MLongV v)             = GVM.basicLength v
  basicUnsafeSlice i n (MLongV v)             = MLongV $ GVM.basicUnsafeSlice i n v
  basicOverlaps (MLongV v1) (MLongV v2)       = GVM.basicOverlaps v1 v2

  basicUnsafeRead  (MLongV v) i               = Longitude `liftM` GVM.basicUnsafeRead v i
  basicUnsafeWrite (MLongV v) i (Longitude x) = GVM.basicUnsafeWrite v i x

  basicClear (MLongV v)                       = GVM.basicClear v
  basicSet   (MLongV v)         (Longitude x) = GVM.basicSet v x

  basicUnsafeNew n                             = MLongV `liftM` GVM.basicUnsafeNew n
  basicUnsafeReplicate n     (Longitude x)     = MLongV `liftM` GVM.basicUnsafeReplicate n x
  basicUnsafeCopy (MLongV v1) (MLongV v2)      = GVM.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MLongV v)   n               = MLongV `liftM` GVM.basicUnsafeGrow v n

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MLongV v)                   = GVM.basicInitialize v
#endif

instance GV.Vector Vector Longitude where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MLongV v)          = LongV  `liftM` GV.basicUnsafeFreeze v
  basicUnsafeThaw (LongV v)             = MLongV `liftM` GV.basicUnsafeThaw v
  basicLength (LongV v)                 = GV.basicLength v
  basicUnsafeSlice i n (LongV v)        = LongV $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM (LongV v) i         = Longitude   `liftM`  GV.basicUnsafeIndexM v i

  basicUnsafeCopy (MLongV mv) (LongV v) = GV.basicUnsafeCopy mv v
  elemseq _ (Longitude x)               = GV.elemseq (undefined :: Vector a) x


----------------------------- Instance for Geo ---------------------------------------------

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


--------------------------------- Instance for Elevated elements -------------------------------

newtype instance MVector s (Elevated a) = MEV (MVector s (a, Double))
newtype instance Vector    (Elevated a) = EV  (Vector (a,Double))

instance Unbox a => GVM.MVector MVector (Elevated a) where
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
  basicLength          (MEV v)       = GVM.basicLength v
  basicUnsafeSlice i n (MEV v)       = MEV $ GVM.basicUnsafeSlice i n v
  basicOverlaps (MEV v1) (MEV v2)    = GVM.basicOverlaps v1 v2

  basicUnsafeRead  (MEV v) i         = uncurry Elevated `liftM` GVM.basicUnsafeRead v i

  basicUnsafeWrite (MEV v) i ea      = GVM.basicUnsafeWrite v i (unElevate ea, altitude ea)

  basicClear (MEV v)                 = GVM.basicClear v
  basicSet   (MEV v)         ea      = GVM.basicSet v (unElevate ea, altitude ea)

  basicUnsafeNew n                   = MEV `liftM` GVM.basicUnsafeNew n
  basicUnsafeReplicate n     ea      = MEV `liftM` GVM.basicUnsafeReplicate n (unElevate ea, altitude ea)
  basicUnsafeCopy (MEV v1) (MEV v2)  = GVM.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MEV v)   n        = MEV `liftM` GVM.basicUnsafeGrow v n

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MEV v)              = GVM.basicInitialize v
#endif

instance Unbox a => GV.Vector Vector (Elevated a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MEV v)       = EV  `liftM` GV.basicUnsafeFreeze v
  basicUnsafeThaw (EV v)          = MEV `liftM` GV.basicUnsafeThaw v
  basicLength (EV v)              = GV.basicLength v
  basicUnsafeSlice i n (EV v)     = EV $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM (EV v) i      = uncurry Elevated `liftM` GV.basicUnsafeIndexM v i

  basicUnsafeCopy (MEV mv) (EV v) = GV.basicUnsafeCopy mv v
  elemseq _ ea                    = GV.elemseq (undefined :: Vector a) (unElevate ea, altitude ea)
