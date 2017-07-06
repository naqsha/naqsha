{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Naqsha.Geometry.Coordinate.GeoHash
       ( GeoHash, encode, decode, toByteString
       ) where

import           Control.Monad            ( liftM         )
import           Data.Bits
import           Data.ByteString          ( ByteString    )
import           Data.ByteString.Internal ( unsafeCreate  )
import           Data.Char                ( ord, chr      )
import           Data.Vector.Unboxed      ( MVector(..), Vector, Unbox )
import           Data.Word                ( Word64, Word8 )
import           Data.Int                 ( Int64         )
import           Foreign.Ptr              ( Ptr           )
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GVM

import Naqsha.Geometry.Internal
import Naqsha.Geometry.Coordinate ( Geo(..) )


-- | The encoding of geo-coordinates as a geohash string. The
data GeoHash = GeoHash  {-# UNPACK #-} !Word64
                        {-# UNPACK #-} !Word64 deriving (Eq, Show)


instance Ord GeoHash where
  (<=)    = priorityApply (<=)
  (<)     = priorityApply (<)
  (>=)    = priorityApply (>=)
  (>)     = priorityApply (>)
  compare = priorityApply compare

{-# INLINE priorityApply #-}

-- | Should the first or the second coordinate be used for comparison.
priorityApply :: (Word64 -> Word64 -> a) -> GeoHash -> GeoHash -> a
priorityApply cmp (GeoHash a0 a1) (GeoHash b0 b1)
  | xor0 >= xor1 = a0 `cmp` b0
  | xor0 > xor01 = a0 `cmp` b0
  | otherwise    = a1 `cmp` b1
  where xor0  = a0 `xor` b0
        xor1  = a1 `xor` b1
        xor01 = xor0 `xor` xor1


------------------------------------------ Base 32 encoding used by geohash --------------------------

-- | A base 32-digit
newtype B32 = B32 Word8

instance Show B32 where
  show b32 = [b32ToChar b32]

-- The digit ranges are
-- 0-9, b-h, jk, mn, p-z
--
-- b - 10
-- c - 11
-- d - 12
-- e - 13
-- f - 14
-- g - 15
-- h - 16
--------- Broken range ---
-- j - 17
-- k - 18
--------- Broken range ----
-- m - 19
-- n - 20
---------- Broken range ---
-- p - 21
-- q - 22
-- r - 23
-- s - 24
-- t - 25
-- u - 26
-- v - 27
-- w - 28
-- x - 29
-- y - 30
-- x - 31

cToB32 :: Char -> Maybe B32
cToB32 x
  | '0'   <= x && x <= '9' = Just $ B32 $ toEnum $ ord x - ord '0'
  | 'b'   <= x && x <= 'h' = Just $ B32 $ toEnum $ ord x - ord 'b' + 10
  | 'p'   <= x && x <= 'z' = Just $ B32 $ toEnum $ ord x - ord 'p' + 21
  | x    == 'j'            = Just $ B32 17
  | x    == 'k'            = Just $ B32 18
  | x    == 'm'            = Just $ B32 19
  | x    == 'n'            = Just $ B32 20
  | otherwise              = Nothing

b32ToChar :: B32 -> Char
b32ToChar b32
  | 0  <= w && w <= 9  = chr (ord '0' + w)
  | 10 <= w && w <= 16 = chr (ord 'b' + w - 10)
  | 21 <= w && w <= 32 = chr (ord 'p' + w - 21)
  | w == 17            = 'j'
  | w == 18            = 'k'
  | w == 19            = 'm'
  | w == 20            = 'n'
  | otherwise          = error "error:b32: fatal this should never happen"
  where w = fromEnum $ b32ToWord8 b32

-- | Convert word8 to base32 by truncation after 5 bits.
word8ToB32 :: Word8 -> B32
word8ToB32 = B32 . (.&. 0x1F)

-- | Convert from base32 to the word8 value.
b32ToWord8 (B32 x) = x .&. 0x1F

------------------------------- Vector instance for B32 --------------


instance Unbox B32
newtype instance MVector s B32 = MVB32 (MVector s Word8)
newtype instance Vector    B32 = VB32  (Vector Word8)

instance GVM.MVector MVector B32 where
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
  basicLength          (MVB32 v)        = GVM.basicLength v
  basicUnsafeSlice i n (MVB32 v)        = MVB32 $ GVM.basicUnsafeSlice i n v
  basicOverlaps (MVB32 v1) (MVB32 v2)   = GVM.basicOverlaps v1 v2

  basicUnsafeRead  (MVB32 v) i          = B32 `liftM` GVM.basicUnsafeRead v i
  basicUnsafeWrite (MVB32 v) i (B32 x)  = GVM.basicUnsafeWrite v i x

  basicClear (MVB32 v)                  = GVM.basicClear v
  basicSet   (MVB32 v)         (B32 x)  = GVM.basicSet v x

  basicUnsafeNew n                      = MVB32 `liftM` GVM.basicUnsafeNew n
  basicUnsafeReplicate n     (B32 x)    = MVB32 `liftM` GVM.basicUnsafeReplicate n x
  basicUnsafeCopy (MVB32 v1) (MVB32 v2) = GVM.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MVB32 v)   n         = MVB32 `liftM` GVM.basicUnsafeGrow v n

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MVB32 v)           = GVM.basicInitialize v
#endif

instance GV.Vector Vector B32 where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MVB32 v)   = VB32  `liftM` GV.basicUnsafeFreeze v
  basicUnsafeThaw (VB32 v)      = MVB32 `liftM` GV.basicUnsafeThaw v
  basicLength (VB32 v)          = GV.basicLength v
  basicUnsafeSlice i n (VB32 v) = VB32 $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM (VB32 v) i  = B32   `liftM`  GV.basicUnsafeIndexM v i

  basicUnsafeCopy (MVB32 mv) (VB32 v) = GV.basicUnsafeCopy mv v
  elemseq _ (B32 x)                    = GV.elemseq (undefined :: Vector a) x




--------------- Encoding and decoding -------------------------


-- | Encode Geo position to the corresponding GeoHash.
encode :: Geo -> GeoHash
encode (Geo x y)  = GeoHash (encLon y) $ encLat x
  where encLat = invertSignShiftLeft . unAngle . unLat

        encLon = invertSign . unAngle . unLong

decode :: GeoHash -> Geo
decode (GeoHash u v) = Geo (decLat v) $ decLon u
  where decLat = Latitude  . Angle . invertSignShiftRight
        decLon = Longitude . Angle . invertSign


invertSignShiftLeft :: (Integral a, Num b, Bits b) => a -> b
invertSignShiftLeft u
  | testBit res 63 =  clearBit resp 63
  | otherwise      =  setBit   resp 63
  where res  = fromIntegral u
        resp = res `shiftL` 1


invertSignShiftRight :: (Integral a, Num b, Bits b) => a -> b
invertSignShiftRight = flip shiftR 1 . invertSign

-- | Sign inversion for adjusting.
invertSign :: (Integral a, Num b, Bits b) => a -> b
invertSign = flip complementBit 63 . fromIntegral

{-# SPECIALIZE invertSign :: Word64 -> Int64  #-}
{-# SPECIALIZE invertSign :: Int64  -> Word64 #-}

foreign import ccall unsafe "naqsha_geohash32"
  c_geohash32 :: Word64 -> Word64 -> Ptr Word8 -> IO ()


-- | Generates a 24-byte, base32 encoding of geohash values. This
-- encoding is a little bit lossy; Latitudes lose lower 3-bits and
-- Longitudes loose lower 4-bits of information.
toByteString :: GeoHash -> ByteString
toByteString (GeoHash x y) = unsafeCreate 26 $ c_geohash32 x y
