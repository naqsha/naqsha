{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Naksha.Core.Position
       ( -- * Latitude and longitude and geopositions.
         -- $latandlong$
         Latitude, Longitude, Geo(..)
       , lat, long, minute, second
       -- ** Some common latitude
       , equator, northPole, southPole
         -- ** Some common longitude
       , greenwich
       -- ** Objects with elevation
       , Elevated, elevate, altitude
       -- * A geographic position.
       , Location(..)
       ) where

import           Control.Monad               ( liftM )
import           Data.Group
import           Data.Int
import           Data.Monoid
import           Data.Vector.Unboxed         ( MVector(..), Vector)
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

newtype Latitude = Latitude { unLat :: Int64 } deriving Eq

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
newtype Longitude = Longitude { unLong :: Int64 }

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

-- | Angle in minutes.
minute  :: Double
minute = 1/60

-- | Angle in seconds.
second :: Double
second = (1/3600)


-- | The coordinates of a point on the earth's surface.
data Geo = Geo {-# UNPACK #-} !Latitude
               {-# UNPACK #-} !Longitude

-- | An object that is elevated, i.e. that comes with an elevation.
data Elevated a = Elevated { unElevate   :: a
                           , altitude :: Double
                           }

-- | Attach an elevation to an object.
elevate  :: a            -- ^ Stuff to which an elevation has to be attached.
         -> Double       -- ^ Elevation in meters from mean-sealevel
         -> Elevated a
elevate  = Elevated


instance Location a => Location (Elevated a) where
  latitude     = latitude    . unElevate
  longitude    = longitude   . unElevate
  geoPosition  = geoPosition . unElevate


instance Eq Geo where
  (==) (Geo xlat xlong) (Geo ylat ylong)
    | xlat == northPole = ylat == northPole  -- longitude irrelevant for north pole
    | xlat == southPole = ylat == southPole  -- longitude irrelevant for south pole
    | otherwise         = xlat == ylat && xlong == ylong


--------------- Helper functions for latitude and longitudes.

-- | The scale of representation of latitude and longitude. Currently
-- it is number 10^7.
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



------------------- Making stuff suitable for unboxed vector. --------------------------

newtype instance MVector s Latitude = MLatV (MVector s Int64)
newtype instance Vector    Latitude = LatV  (Vector Int64)


newtype instance MVector s Longitude = MLongV (MVector s Int64)
newtype instance Vector    Longitude = LongV  (Vector Int64)


newtype instance MVector s Geo = MGeoV (MVector s (Int64,Int64))
newtype instance Vector    Geo = GeoV  (Vector    (Int64,Int64))


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
