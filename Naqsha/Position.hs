{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
-- | This module captures position of a point on the globe.
module Naqsha.Position
       ( -- * Latitude, longitude and geopositions.
         -- $latandlong$
         Geo(..)
       , northPole, southPole
       , Latitude, Longitude, lat, lon
       , equator, greenwich
       ) where

import           Control.Monad               ( liftM )
import           Data.Default
import           Data.Fixed
import           Data.Monoid
import           Data.Group
import           Data.Vector.Unboxed         ( MVector(..), Vector)
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GVM
import           Text.Read

import           Prelude         -- To avoid redundunt import warnings.

import           Naqsha.Geometry.Angle

-- $latandlong$
--
-- A point on the globe is specified by giving its geo coordinates
-- captures by the type `Geo`.  It is essentially a pair of the
-- `Latitude` and `Longitude` of the point.
--
-- == Examples
--
-- > kanpurLatitude  :: Latitude
-- > kanpurLatitude  = lat $ degree 26.4477777
-- > kanpurLongitude :: Longitude
-- > kanpurLongitude = lon $ degree 80.3461111
--
--
-- > kanpurLatitude  = lat $ degree 26 <> minute 26 <> second 52
-- > kanpurLongitude = lon $ degree 80 <> minute 20 <> second 46
--
-- The show and read instance of the `Latitude` and `Longitude` types
-- uses degrees for displaying and reading respectively. Show and Read
-- instances can express these quantities up to Nano degree precision.
--


----------------------------- Lattitude ----------------------------------

-- | The latitude of a point. Positive denotes North of Equator where
-- as negative South.
newtype Latitude = Latitude { unLat :: Angle } deriving (Eq, Ord)

-- | Construct latitude out of an angle.
lat :: Angle -> Latitude
lat = Latitude . normLat

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


instance Angular Latitude where
  toAngle = unLat

instance Show Latitude where
  show = show . (toDegree :: Angle -> Nano) . unLat

instance Read Latitude where
  readPrec = conv <$> readPrec
    where conv = lat . degree . (toRational :: Nano -> Rational)


instance Default Latitude where
  def = equator

-- | The latitude of equator.
equator :: Latitude
equator = lat $ degree 0

-- | The latitude corresponding to the Tropic of Cancer.
tropicOfCancer :: Latitude
tropicOfCancer = north $ degree 23.5

-- | The latitude corresponding to the Tropic of Capricon
tropicOfCapricon :: Latitude
tropicOfCapricon = south $ degree 23.5


instance Bounded Latitude where
  maxBound = lat $ degree 90
  minBound = lat $ degree (-90)


-------------------------- Longitude ------------------------------------------

-- | The longitude of a point. Positive denotes East of the Greenwich
-- meridian where as negative denotes West.
newtype Longitude = Longitude { unLong :: Angle }
  deriving (Eq, Bounded, Default, Angular, Ord, Monoid, Group)

instance Show Longitude where
  show = show . (toDegree :: Angle -> Nano) . unLong

instance Read Longitude where
  readPrec = conv <$> readPrec
    where conv  = lon . degree . (toRational :: Nano -> Rational)

-- | Convert angles to longitude.
lon :: Angle -> Longitude
lon = Longitude

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

-- | The coordinates of a point on the earth's surface.
data Geo = Geo {-# UNPACK #-} !Latitude
               {-# UNPACK #-} !Longitude
         deriving Show

instance Default Geo where
  def = Geo def def


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

-- | normalise latitude values.
normLat :: Angle -> Angle
normLat ang | degree (-90)  <= ang && ang < degree 90 = ang
            | ang > degree 90                         = succ (maxBound  <> invert ang)
            | otherwise                               = minBound <> invert ang


--------------------------- Internal helper functions ------------------------


newtype instance MVector s Latitude = MLatV (MVector s Angle)
newtype instance Vector    Latitude = LatV  (Vector Angle)


newtype instance MVector s Longitude = MLongV (MVector s Angle)
newtype instance Vector    Longitude = LongV  (Vector Angle)


newtype instance MVector s Geo = MGeoV (MVector s (Angle,Angle))
newtype instance Vector    Geo = GeoV  (Vector    (Angle,Angle))


-------------------- Instance for Angle --------------------------------------------


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
