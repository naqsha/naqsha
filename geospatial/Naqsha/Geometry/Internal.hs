{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

-- | The internal module that exposes the basic geometric types in
-- naqsha. This interface is subject to change and hence use with
-- caution.
module Naqsha.Geometry.Internal
  ( Angle(..)
  , degree , minute, second
  , radian
  , toDegree, toRadian
  , Latitude(..), Longitude(..), lat, lon
  , Elevation(..),
  ) where

-- Ugly hack to prevent pre-7.10 ghc warnings

import           Control.Applicative         ( (<$>) )
import           Control.Monad               ( liftM )
import           Data.Bits                   ( Bits  )
import           Data.Fixed
import           Data.Group
import           Data.Int
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid     hiding      ((<>))
import           Data.Semigroup
#endif
import           GHC.Real
import           Data.Vector.Unboxed         ( MVector(..), Vector, Unbox)
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GVM
import           Text.Read

----------------------------- Angles and Angular quantities -----------------------

-- | An abstract angle. Internally, angles are represented as a 64-bit
-- integer with each unit contribute 1/2^64 fraction of a complete
-- circle. This means that angles are accurate up to a resolution of 2
-- π / 2^64 radians. Angles form a group under the angular addition
-- and the fact that these are represented as integers means one can
-- expect high speed accurate angle arithmetic.
--
-- When expressing angles one can use a more convenient notation:
--
-- > myAngle   = degree 21.71167
-- > yourAngle = degree 21 <> minute 42 <> second 42
--
newtype Angle = Angle {unAngle :: Int64} deriving (Enum, Eq, Ord, Unbox, Show, Read, Bits)

instance Semigroup Angle where
  (<>)  (Angle x)  (Angle y) = Angle $ x + y

instance Monoid Angle where
  mempty  = Angle 0
  mappend = (<>)
  mconcat = Angle . sum . map unAngle

instance Group Angle where
  invert (Angle x) = Angle $ negate x

instance Bounded Angle where
  maxBound = Angle maxBound
  minBound = Angle minBound

-- | Express angle in degrees.
degree :: Rational -> Angle
degree = Angle  . fromInteger  . round . (*scale)
  where scale = (2^(64:: Int)) % 360

-- | Express angle in minutes.
minute :: Rational -> Angle
minute = degree . (*scale)
  where scale = 1 % 60

-- | Express angle in seconds.
second :: Rational -> Angle
second = degree . (*scale)
    where scale = 1 % 3600

-- | Express angle in radians
radian  :: Double -> Angle
radian  = Angle . round . (*scale)
  where scale = (2^(63:: Int)) / pi


---------------------- Decimal representation of angle ----------------------------------

-- | Measure angle in degrees. This conversion may lead to loss of
-- precision.
toDegree :: Fractional r => Angle -> r
toDegree  = fromRational  . (*conv) . toRational . unAngle
  where conv = 360 % (2^(64  :: Int))

-- | Measure angle in radians. This conversion may lead to loss of
-- precision.
toRadian :: Angle -> Double
toRadian = (*conv) . fromIntegral . unAngle
  where conv = pi / (2^(63:: Int))

------------------- Making stuff suitable for unboxed vector. --------------------------

newtype instance MVector s Angle = MAngV  (MVector s Int64)
newtype instance Vector    Angle = AngV   (Vector Int64)


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

------------------------------------- Latitude and Longitude ---------------------------------


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


-- | Construct latitude out of an angle.
lat :: Angle -> Latitude
lat = Latitude . normLat


-- | normalise latitude values.
normLat :: Angle -> Angle
normLat ang | degree (-90)  <= ang && ang < degree 90 = ang
            | ang > degree 90                         = succ (maxBound  <> invert ang)
            | otherwise                               = minBound <> invert ang


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

--------------------------- Elevation -----------------------------------------

-- | Distance of a point on the surface of the glob from mean sea
-- level (in metres).
newtype Elevation = Elevation Double


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
