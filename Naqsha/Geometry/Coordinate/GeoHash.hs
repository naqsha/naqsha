module Naqsha.Geometry.Coordinate.GeoHash
       ( GeoHash, encode, decode
       ) where

import Data.Word
import Data.Bits

import Naqsha.Geometry.Internal
import Naqsha.Geometry.Coordinate ( Geo(..) )

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
encode (Geo x y) = GeoHash (latToWord63 x) $ lonToWord64 y


decode :: GeoHash -> Geo
decode (GeoHash u v) = Geo (word63ToLat u) $ word64ToLon v

-- | Helper function to convert signed latitude to its geohash
-- code. There is only 63 bits of precision because the angle varies
-- from -90 to 90.
latToWord63 :: Latitude -> Word64
latToWord63 (Latitude (Angle ang))
  | ang < 0   = clearBit magBits 63
  | otherwise = setBit   magBits 63
  where magBits = fromIntegral ang `shiftL` 1


-- | Inverse geohash latitude to angular latitude
word63ToLat :: Word64 -> Latitude
word63ToLat w64 | testBit w64 63 =  Latitude $ Angle $ magBits
                | otherwise      =  Latitude $ Angle $ magBits .|. mask
  where magBits = fromIntegral $ clearBit w64 63  `shiftR` 1
        mask    = 0x3 `shiftL` 62

-- | Helper function to convert the signed longitude to an appropriate
-- 64-bit geohash code.
lonToWord64 :: Longitude -> Word64
lonToWord64 (Longitude (Angle ang)) = flip complementBit 63 $ fromIntegral ang


-- | Inverse geohash latitude to angular latitude
word64ToLon :: Word64 -> Longitude
word64ToLon = Longitude . Angle . fromIntegral . flip complementBit 63
