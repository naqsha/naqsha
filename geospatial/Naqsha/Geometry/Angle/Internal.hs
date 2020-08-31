{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

-- | The internal module that exposes the internal representation of
-- angles. One should use this only if absolutely required as this
-- breaks type safety and the interface is subject to change.
module Naqsha.Geometry.Angle.Internal
  ( Angle(..), resolution
  , degree , minute, second
  , radian
  , toDegree, toRadian
  , Angular(..)
  ) where

import           Naqsha.Prelude
import           GHC.Real
import           Data.Vector.Unboxed         ( MVector(..), Vector, Unbox)
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GVM

----------------------------- Angles and Angular quantities -----------------------

-- | An abstract angle. Internally, angles are represented as a 64-bit
-- integer with each unit contribute 1/2⁶⁴ fraction of a complete
-- circle. This means that angles are accurate up to a resolution of 2
-- π / 2⁶⁴ radians. Angles form a group under the angular addition
-- and the fact that these are represented as integers means one can
-- expect high speed accurate angle arithmetic.
--
-- When expressing angles one can use a more convenient notation:
--
-- > myAngle   = degree 21.71167
-- > yourAngle = degree 21 <> minute 42 <> second 42
--
newtype Angle = Angle {unAngle :: Int64} deriving (Enum, Eq, Ord, Unbox, Show, Read, Bits)


-- | The smallest non-zero positive angle that can be represented in the library.
resolution :: Angle
resolution = Angle 1

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

------------------------------ The angular class ------------------------

-- | Angular quantities.
class Angular a where
  toAngle   :: a -> Angle

instance Angular Angle where
  toAngle = id

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

  basicUnsafeRead  (MAngV v) i            = Angle <$> GVM.basicUnsafeRead v i
  basicUnsafeWrite (MAngV v) i (Angle x)  = GVM.basicUnsafeWrite v i x

  basicClear (MAngV v)                    = GVM.basicClear v
  basicSet   (MAngV v)         (Angle x)  = GVM.basicSet v x

  basicUnsafeNew n                        = MAngV <$> GVM.basicUnsafeNew n
  basicUnsafeReplicate n     (Angle x)    = MAngV <$> GVM.basicUnsafeReplicate n x
  basicUnsafeCopy (MAngV v1) (MAngV v2)   = GVM.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MAngV v)   n           = MAngV <$> GVM.basicUnsafeGrow v n
  basicInitialize (MAngV v)               = GVM.basicInitialize v


instance GV.Vector Vector Angle where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MAngV v)         = AngV  <$> GV.basicUnsafeFreeze v
  basicUnsafeThaw (AngV v)            = MAngV <$> GV.basicUnsafeThaw v
  basicLength (AngV v)                = GV.basicLength v
  basicUnsafeSlice i n (AngV v)       = AngV $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM (AngV v) i        = Angle   <$>  GV.basicUnsafeIndexM v i

  basicUnsafeCopy (MAngV mv) (AngV v) = GV.basicUnsafeCopy mv v
  elemseq _ (Angle x)                 = GV.elemseq (undefined :: Vector a) x
