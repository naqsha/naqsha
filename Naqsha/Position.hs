{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | This module captures position of a point on the globe.
module Naqsha.Position
       ( -- * Latitude, longitude and geopositions.
         -- $latandlong$
         Latitude, Longitude, lat, lon, Geo(..)
       , Location(..)
         -- ** Some common latitude
       , equator, northPole, southPole
         -- ** Some common longitude
       , greenwich
         -- * Angles and angular quantities.
       , Angle, Angular(..)
         -- * A geographic bound.
       , GeoBounds, maxLatitude, maxLongitude, minLatitude, minLongitude
       -- ** Distance calculation.
       , dHvS, dHvS', rMean
       ) where

import           Control.Lens
import           Control.Monad               ( liftM )
import           Data.Default
import           Data.Fixed
import           Data.Int
import           Data.Monoid
import           Data.Group
import           Data.Vector.Unboxed         ( MVector(..), Vector, Unbox)
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GVM
import           Text.Read

import           Prelude         -- To avoid redundunt import warnings.


-- $latandlong$
--
-- A point on the globe is specified by giving its geo coordinates
-- captures by the type `Geo`.  It is essentially a pair of the
-- `Latitude` and `Longitude` of the point.
--
-- The default representation of Latitude and Longitudes are in
-- degrees.
--
-- > kanpurLatitude  :: Latitude
-- > kanpurLatitude  = lat 26.4477777
-- > kanpurLongitude :: Longitude
-- > kanpurLongitude = lon 80.3461111
--
-- Latitudes and longitudes are instances of `Monoid` where the monoid
-- instance adds up the angle. One can use this `Monoid` instance to
-- express in degrees, minutes and seconds as follows
--
-- > kanpurLatitude  = lat 26 <> minute 26 <> second 52
-- > kanpurLongitude = lon 80 <> minute 20 <> second 46
--
-- They are also instances of `Group` where `invert` is the angle in
-- the opposite direction, i.e for latitudes, `invert` converts from
-- North to South and vice-versa and for longitudes `invert` converts
-- from East to West. Be careful with negative quantities though. To
-- express a latitude of -1° 2′ 3″ one should use
--
-- > someNegLatitude :: Latitude
-- > someNegLatitude = invert $ lat 1 <> minute 2 <> second 3  -- correct
--
-- and not
--
-- > someNegLatitude = lat (-1) <> minute 2 <> second 3  -- wrong
--
-- We would like to attach additional information with geographic
-- locations. The type class `Location` captures all types that have
-- an associated geographical coordinates.

----------------------------- Lattitude ----------------------------------

-- | The latitude of a point. Positive denotes North of Equator where
-- as negative South.

newtype Latitude = Latitude { unLat :: Angle }

-- | Construct latitude out of an angle.
lat :: Angle -> Latitude
lat = Latitude

instance Show Latitude where
  show = show . unLat . normalise

instance Read Latitude where
  readsPrec n = map conv . readsPrec n
    where conv (ang,s) = (Latitude ang, s)


instance Angular Latitude where
  normalise = normLat  . unLat
  minutes   = Latitude . minutes
  seconds   = Latitude . seconds
  rad       = Latitude . rad
  toRad     = toRad . normLat . unLat

instance Eq Latitude where
  (==) l1 l2 = unLat (normalise l1) == unLat (normalise l2)

instance Ord Latitude where
  compare x y = unLat (normalise x) `compare` unLat (normalise y)

instance Monoid Latitude where
  mempty      = equator
  mappend x y = Latitude $ unLat x + unLat y

instance Group Latitude where
  invert  = Latitude . negate . unLat

instance Default Latitude where
  def = equator

-- | The latitude of equator.
equator :: Latitude
equator = lat 0

-- | The latitude of north pole.
northPole :: Latitude
northPole = lat 90

-- | The latitude of south pole.
southPole :: Latitude
southPole = lat (-90)

instance Bounded Latitude where
  maxBound = northPole
  minBound = southPole


-------------------------- Longitude ------------------------------------------

-- | The longitude of a point. Positive denotes East of the Greenwich
-- meridian where as negative denotes West.
newtype Longitude = Longitude { unLong :: Angle }
  deriving (Eq,Bounded, Default, Angular, Ord, Monoid, Group)

instance Show Longitude where
  show = show . unLong

instance Read Longitude where
  readsPrec n = map conv . readsPrec n
    where conv (ang, s) = (lon ang, s)

-- | Convert angles to longitude.
lon :: Angle-> Longitude
lon = Longitude


-- | The zero longitude.
greenwich :: Longitude
greenwich = lon 0

-- | The coordinates of a point on the earth's surface.
data Geo = Geo {-# UNPACK #-} !Latitude
               {-# UNPACK #-} !Longitude
         deriving Show

instance Monoid Geo where
  mempty      = Geo mempty mempty
  mappend (Geo xlat xlong) (Geo ylat  ylong) = Geo (xlat `mappend` ylat) (xlong `mappend` ylong)

instance Group Geo where
  invert (Geo lt lg) = Geo (invert lt) $ invert lg

instance Default Geo where
  def = Geo def def

-- | Objects that have a location on the globe. Minimum complete
-- implementation: either the two functions `longitude` and `latitude`
-- or the single function `geoPosition`.
class Location a where
  -- | The latitude of the object.
  latitude    :: Lens' a Latitude

  -- | The longitude of the object.
  longitude   :: Lens' a Longitude

  -- | The geo-Position of the object.
  geoPosition :: Lens' a Geo

  latitude = geoPosition . latitude

  longitude = geoPosition . longitude

----------------------------- Angles and Angular quantities -----------------------

-- | An abstract angle measured in degrees up to some precision
-- (system dependent).
newtype Angle = Angle {unAngle ::  Int64} deriving Unbox

instance Enum Angle where
   toEnum   = fromAngEnc . toEnum
   fromEnum = fromEnum   . toAngEnc . normalise

instance Default Angle where
  def = 0

instance Angular Angle where
  normalise ang | r > oneEighty = Angle $ r - threeSixty
                | otherwise     = Angle r
    where r = unAngle ang `rem` threeSixty
          threeSixty =  360 * scale
          oneEighty  =  180 * scale

  minutes   = fromAngEnc . (/60) . fromIntegral
  seconds   = fromDouble . (/3600)
  rad       = fromDouble . (/pi) . (*180)
  toRad     = (*pi) . (/180) . toDouble
  {-# INLINE normalise #-}

instance Num Angle where
  x + y         = normalise $ Angle $ unAngle x + unAngle y
  x - y         = normalise $ Angle $ unAngle x - unAngle y
  x * y         = normalise $ Angle $ unAngle x * unAngle y
  negate        = Angle . negate . unAngle
  abs           = Angle . abs    . unAngle
  signum        = fromIntegral   . signum . unAngle  -- Angle . abs . unAngle will result in 1 * 10^9
  fromInteger   = fromAngEnc     . fromInteger

instance Show Angle where
  show = show . toAngEnc

instance Read Angle where
  readsPrec n = map conv . readsPrec n
    where conv (x,s) = (fromAngEnc x, s)

instance Eq Angle where
  (==) x y = unAngle (normalise x) == unAngle (normalise y)

instance Ord Angle where
  compare x y = unAngle (normalise x) `compare` unAngle (normalise y)

instance Monoid Angle where
  mempty      = 0
  mappend     = (+)

instance Group Angle where
  invert = negate

instance Bounded Angle where
  maxBound = fromAngEnc 180
  minBound = fromAngEnc $ succ (-180)

----------------- Encoding Angles ---------------------------------------------------

-- | Angles are encoded in nano-degrees.
type AngEnc = Nano

toAngEnc :: Angle -> AngEnc
toAngEnc = MkFixed . toInteger . unAngle

fromAngEnc :: AngEnc  -> Angle
fromAngEnc a@(MkFixed x) =  Angle $ fromIntegral $ x `rem` (360 * resolution a)

scale :: Num a => a
scale = fromIntegral $ resolution (1 :: AngEnc)

fromDouble :: Double -> Angle
fromDouble = fromAngEnc . fromInteger . round . (*scale)

toDouble :: Angle -> Double
toDouble = (/scale) . fromIntegral . unAngle

------------------------------ The angular class ------------------------


-- | Angular quantities.
class Angular a where
  -- | Express angular quantity in minutes.
  minutes :: Int -> a

  -- | Express angular quantity in seconds.
  seconds :: Double -> a

  -- | Express angular quantity in radians
  rad  :: Double -> a

  -- | Get the angle in radians.
  toRad :: a -> Double

  -- | Normalise the quantity
  normalise  :: a -> a

instance Location Geo where
  latitude  = lens getter setter
    where setter (Geo _ lo) la = Geo la lo
          getter (Geo la _)    = la

  longitude = lens (\ (Geo _ lo) -> lo) (\ (Geo la _) lo -> Geo la lo)
  geoPosition  = lens id (\ _ x -> x)


instance Eq Geo where
  (==) (Geo xlat xlong) (Geo ylat ylong)
    | xlat == northPole = ylat == northPole  -- longitude irrelevant for north pole
    | xlat == southPole = ylat == southPole  -- longitude irrelevant for south pole
    | otherwise         = xlat == ylat && xlong == ylong

--------------------- Distance calculation -------------------------------------

-- | Mean earth radius in meters. This is the radius used in the
-- haversine formula of `dHvs`.
rMean  :: Double
rMean = 6371008


-- | This combinator computes the distance (in meters) between two geo-locations
-- using the haversine distance between two points. For `Position` which have an
dHvS :: ( Location geo1
        , Location geo2
        )
      => geo1   -- ^ Point 1
      -> geo2   -- ^ Point 2
      -> Double -- ^ Distance in meters.
dHvS = dHvS' rMean

{-# SPECIALISE dHvS :: Geo      -> Geo      -> Double #-}

-- | A generalisation of `dHvS` that takes the radius as
-- argument. Will work on Mars for example once we set up a latitude
-- longitude system there. For this function units does not matter ---
-- the computed distance is in the same unit as the input radius. We have
--
-- > dHvS = dHvS' rMean
--
dHvS' :: ( Location geo1
         , Location geo2
         )
      => Double  -- ^ Radius (in whatever unit)
      -> geo1     -- ^ Point 1
      -> geo2     -- ^ Point 2
      -> Double
{-# SPECIALISE dHvS' :: Double -> Geo      -> Geo      -> Double #-}
dHvS' r g1 g2 = r * c
  where p1    = toRad $ g1 ^. latitude
        l1    = toRad $ g1 ^. longitude
        p2    = toRad $ g2 ^. latitude
        l2    = toRad $ g2 ^. longitude
        dp    = p2 - p1
        dl    = l2 - l1
        a     = sin (dp/2.0) ^ (2 :: Int) + cos p1 * cos p2 * (sin (dl/2) ^ (2 :: Int))
        c     = 2 * atan2 (sqrt a) (sqrt (1 - a))

--------------------------- Internal helper functions ------------------------


-- | Function to normalise latitudes. It essentially is a saw-tooth
-- function of period 360 with max values 90.
normLat :: Angle -> Latitude
normLat ang = Latitude $ fromAngEnc $ signum y * normPosLat (abs y)
  where y = toAngEnc ang

-- | Normalise a positive latitude.
normPosLat :: AngEnc -> AngEnc
normPosLat x | x <= 90   = x
             | x <= 270  = 180 - x
             | otherwise = x  - 360

------------------- Making stuff suitable for unboxed vector. --------------------------

newtype instance MVector s Angle = MAngV  (MVector s Int64)
newtype instance Vector    Angle = AngV   (Vector Int64)

newtype instance MVector s Latitude = MLatV (MVector s Angle)
newtype instance Vector    Latitude = LatV  (Vector Angle)


newtype instance MVector s Longitude = MLongV (MVector s Angle)
newtype instance Vector    Longitude = LongV  (Vector Angle)


newtype instance MVector s Geo = MGeoV (MVector s (Angle,Angle))
newtype instance Vector    Geo = GeoV  (Vector    (Angle,Angle))


-------------------- Instance for Angle --------------------------------------------

instance GVM.MVector MVector Angle where
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
  basicLength          (MAngV v)          = GVM.basicLength v
  basicUnsafeSlice i n (MAngV v)          = MAngV $ GVM.basicUnsafeSlice i n v
  basicOverlaps (MAngV v1) (MAngV v2)     = GVM.basicOverlaps v1 v2

  basicUnsafeRead  (MAngV v) i            = Angle `liftM` GVM.basicUnsafeRead v i
  basicUnsafeWrite (MAngV v) i (Angle x)  = GVM.basicUnsafeWrite v i x

  basicClear (MAngV v)                    = GVM.basicClear v
  basicSet   (MAngV v)         (Angle x)  = GVM.basicSet v x

  basicUnsafeNew n                        = MAngV `liftM` GVM.basicUnsafeNew n
  basicUnsafeReplicate n     (Angle x)    = MAngV `liftM` GVM.basicUnsafeReplicate n x
  basicUnsafeCopy (MAngV v1) (MAngV v2)   = GVM.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MAngV v)   n           = MAngV `liftM` GVM.basicUnsafeGrow v n

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MAngV v)               = GVM.basicInitialize v
#endif

instance GV.Vector Vector Angle where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MAngV v)         = AngV  `liftM` GV.basicUnsafeFreeze v
  basicUnsafeThaw (AngV v)            = MAngV `liftM` GV.basicUnsafeThaw v
  basicLength (AngV v)                = GV.basicLength v
  basicUnsafeSlice i n (AngV v)       = AngV $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM (AngV v) i        = Angle   `liftM`  GV.basicUnsafeIndexM v i

  basicUnsafeCopy (MAngV mv) (AngV v) = GV.basicUnsafeCopy mv v
  elemseq _ (Angle x)                 = GV.elemseq (undefined :: Vector a) x


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




-- | A boundary on earth given by the range of latitude and
-- longitude. We represent this as a pair of Geo coordinates. The
-- `minGeo` given the minimum latitude and longitude, whereas `maxGeo`
-- gives the maximum latitude and longitude. If we visualise it as a
-- rectangle (which is not really accurate because we are on a globe),
-- `minGeo` gives the left bottom corner and `maxGeo` gives the right
-- upper corner.
data GeoBounds = GeoBounds { __maxLatitude  :: Latitude
                           , __minLatitude  :: Latitude
                           , __maxLongitude :: Longitude
                           , __minLongitude :: Longitude
                           } deriving (Show, Eq)

makeLenses ''GeoBounds

-- | The upperbound on latitude
maxLatitude :: Lens' GeoBounds Latitude
maxLatitude = _maxLatitude

-- | The lowerbound on latitude
minLatitude :: Lens' GeoBounds Latitude
minLatitude = _minLatitude

-- | The upperbound on longitude
maxLongitude :: Lens' GeoBounds Longitude
maxLongitude = _maxLongitude

-- | The lowerbound on longitude
minLongitude :: Lens' GeoBounds Longitude
minLongitude = _minLongitude


instance Default GeoBounds where
  def = GeoBounds def def def def
