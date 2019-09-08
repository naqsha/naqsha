{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

-- | The internal module that exposes the latitude and longitude
-- types. Import it only if absolutely required.

module Naqsha.Geometry.LatLon.Internal
  ( Latitude(..)
  , lat, lon
  , north, south
  , equator
  , tropicOfCancer
  , tropicOfCapricon
  , Longitude(..)
  , east, west
  , greenwich
  , LatLon(..)
  , northPole, southPole
  ) where

import           Control.Monad               ( liftM )
import           Data.Bits                   ( Bits  )
import           Data.Group                
import           Data.Fixed
import           Data.Vector.Unboxed         ( MVector(..), Vector)
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GVM
import           Text.Read

import           Naqsha.Geometry.Angle.Internal

-- | The latitude of a point. Positive denotes North of Equator where
-- as negative South.
newtype Latitude = Latitude { unLat :: Angle } deriving (Eq, Ord, Bits)

instance Show Latitude where
  show = show . (toDegree :: Angle -> Nano) . unLat

instance Read Latitude where
  readPrec = conv <$> readPrec
    where conv = lat . degree . (toRational :: Nano -> Rational)

instance Bounded Latitude where
  maxBound = lat $ degree 90
  minBound = lat $ degree (-90)


instance Angular Latitude where
  toAngle = unLat

-- | Construct latitude out of an angle.
lat :: Angle -> Latitude
lat = Latitude . normLat


-- | normalise latitude values.
normLat :: Angle -> Angle
normLat ang | degree (-90)  <= ang && ang < degree 90 = ang
            | ang > degree 90                         = succ (maxBound  <> invert ang)
            | otherwise                               = minBound <> invert ang




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

-------------------------- Longitude ------------------------------------------

-- | The longitude of a point. Positive denotes East of the Greenwich
-- meridian where as negative denotes West.
newtype Longitude = Longitude { unLong :: Angle }
  deriving (Eq, Bounded, Ord, Semigroup, Monoid, Group, Bits)

-- | Convert angles to longitude.
lon :: Angle -> Longitude
lon = Longitude


instance Show Longitude where
  show = show . (toDegree :: Angle -> Nano) . unLong

instance Read Longitude where
  readPrec = conv <$> readPrec
    where conv  = lon . degree . (toRational :: Nano -> Rational)


instance Angular Longitude where
  toAngle = unLong


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

--------------------------- Internal helper functions ------------------------


newtype instance MVector s Latitude = MLatV (MVector s Angle)
newtype instance Vector    Latitude = LatV  (Vector Angle)


newtype instance MVector s Longitude = MLongV (MVector s Angle)
newtype instance Vector    Longitude = LongV  (Vector Angle)


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



------------------- The geometric coordinates. -----------------

-- | The coordinates of a point on the earth's surface.
data LatLon = LatLon {-# UNPACK #-} !Latitude
                     {-# UNPACK #-} !Longitude
         deriving Show


-- | The North pole
northPole :: LatLon
northPole = LatLon maxBound $ lon $ degree 0

-- | The South pole
southPole :: LatLon
southPole = LatLon minBound $ lon $ degree 0

instance Eq LatLon where
  (==) (LatLon xlat xlong) (LatLon ylat ylong)
    | xlat == maxBound = ylat == maxBound  -- longitude irrelevant for north pole
    | xlat == minBound = ylat == minBound  -- longitude irrelevant for south pole
    | otherwise     = xlat == ylat && xlong == ylong

----------------------------- Vector Instance for LatLon ---------------------------------------------


newtype instance MVector s LatLon = MLatLonV (MVector s (Angle,Angle))
newtype instance Vector    LatLon = LatLonV  (Vector    (Angle,Angle))


instance GVM.MVector MVector LatLon where
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
  basicLength          (MLatLonV v)         = GVM.basicLength v
  basicUnsafeSlice i n (MLatLonV v)         = MLatLonV $ GVM.basicUnsafeSlice i n v
  basicOverlaps (MLatLonV v1) (MLatLonV v2) = GVM.basicOverlaps v1 v2

  basicUnsafeRead  (MLatLonV v) i           = do (x,y) <- GVM.basicUnsafeRead v i
                                                 return $ LatLon (Latitude x) $ Longitude y
  basicUnsafeWrite (MLatLonV v) i (LatLon x y) = GVM.basicUnsafeWrite v i (unLat x, unLong y)

  basicClear (MLatLonV v)                      = GVM.basicClear v
  basicSet   (MLatLonV v)         (LatLon x y) = GVM.basicSet v (unLat x, unLong y)

  basicUnsafeNew n                       = MLatLonV `liftM` GVM.basicUnsafeNew n
  basicUnsafeReplicate n   (LatLon x y)  = MLatLonV `liftM` GVM.basicUnsafeReplicate n (unLat x, unLong y)
  basicUnsafeCopy (MLatLonV v1) (MLatLonV v2)  = GVM.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MLatLonV v)   n             = MLatLonV `liftM` GVM.basicUnsafeGrow v n

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MLatLonV v)              = GVM.basicInitialize v
#endif

instance GV.Vector Vector LatLon where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MLatLonV v)         = LatLonV  `liftM` GV.basicUnsafeFreeze v
  basicUnsafeThaw (LatLonV v)            = MLatLonV `liftM` GV.basicUnsafeThaw v
  basicLength (LatLonV v)                = GV.basicLength v
  basicUnsafeSlice i n (LatLonV v)       = LatLonV $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM (LatLonV v) i        = do (x,y) <- GV.basicUnsafeIndexM v i
                                              return $ LatLon (Latitude x) $ Longitude y

  basicUnsafeCopy (MLatLonV mv) (LatLonV v) = GV.basicUnsafeCopy mv v
  elemseq _ (LatLon x y)                    = GV.elemseq (undefined :: Vector a) (unLat x, unLong y)
