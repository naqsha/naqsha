{-# LANGUAGE ForeignFunctionInterface   #-}
module Naqsha.Geometry.Coordinate.GeoHash
       ( GeoHash, encode, decode, toByteString
       ) where

import Data.Word                ( Word64, Word8 )
import Data.Bits
import Data.ByteString          ( ByteString    )
import Data.ByteString.Internal ( unsafeCreate  )
import  Data.Int                ( Int64         )
import Foreign.Ptr              ( Ptr           )
import Naqsha.Geometry.Internal
import Naqsha.Geometry.Coordinate ( Geo(..) )


-- | The encoding of geo-coordinates as a geohash string. The
data GeoHash = GeoHash  {-# UNPACK #-} !Word64
                        {-# UNPACK #-} !Word64 deriving (Eq, Show)


instance Ord GeoHash where
  (<=) (GeoHash a0 a1)(GeoHash b0 b1)
    | cmp0 >= cmp1 = a0 <= b0
    | otherwise    = a1 <= b1
    where cmp0 = a0 `xor` b0
          cmp1 = a1 `xor` b1

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
