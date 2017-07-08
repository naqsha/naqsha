module Naqsha.Geometry.Coordinate.GeoHash
       ( GeoHash, encode, decode, accuracy
       ) where


import           Data.Bits
import qualified Data.ByteString as B
import           Data.Char                ( ord, chr      )
import           Data.String
import           Data.Monoid              ( (<>)          )
import           Data.Word                (  Word8 )



import Naqsha.Geometry.Internal
import Naqsha.Geometry.Coordinate ( Geo(..) )



-- | Precision of encoding measured in base32 digits.
accuracyBase32 :: Int
accuracyBase32 = 12

-- | Precision of encoding measured in bits. We use 64-bit integers
-- for angles and in latitude one of the bits is redundant. So this
-- quantity should be less than 63.
accuracy :: Int
accuracy = accuracyBase32 * 5

-- | The length of the output.
outputLength :: Int
outputLength = 2 * accuracyBase32

-- | The encoding of geo-coordinates as a geohash string. The
data GeoHash = GeoHash B.ByteString deriving (Eq, Ord)

instance Show GeoHash where
  show (GeoHash x) = map b32ToChar $ B.unpack x

instance IsString GeoHash where
  fromString = GeoHash . B.pack . map cToB32 . take 24

------------------------------------------ Base 32 encoding used by geohash --------------------------

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

cToB32 :: Char -> Word8
cToB32 x
  | '0'   <= x && x <= '9' = toEnum $ ord x - ord '0'
  | 'b'   <= x && x <= 'h' = toEnum $ ord x - ord 'b' + 10
  | 'p'   <= x && x <= 'z' = toEnum $ ord x - ord 'p' + 21
  | x    == 'j'            = 17
  | x    == 'k'            = 18
  | x    == 'm'            = 19
  | x    == 'n'            = 20
  | otherwise              = error $ "geohash: bad character " ++ show x

b32ToChar :: Word8 -> Char
b32ToChar b32
  | 0  <= w && w <= 9  = chr (ord '0' + w)
  | 10 <= w && w <= 16 = chr (ord 'b' + w - 10)
  | 21 <= w && w <= 32 = chr (ord 'p' + w - 21)
  | w == 17            = 'j'
  | w == 18            = 'k'
  | w == 19            = 'm'
  | w == 20            = 'n'
  | otherwise          = error "geohash: fatal this should never happen"
  where w = fromEnum $ b32 .&. 0x1F

-- Geohash encoding
-- ---------------
--
-- Notice that the bits of the geohash encoding is essentially got by
-- iterleaving the bits of the longitude and the latitude. However,
-- the first bit is 0 for negative angles 1 for positive
-- angles. Therefore we need to complement the sign bit. Since
-- longitudes vary over the entire range of angles, this is all we
-- need to do for adjustment before interleaving the bits.
--
-- However, the latitudes like in the range -90 to +90. If we ignore the
-- +90 angle then we have the following property of its bit pattern
--
-- 1. Every positive angle (other than +90) is of the form 00xxxxxxx.
--
-- 2. Every negative angle is of the form 11xxxxxxx.
--
-- Therefore, to get the actual bits that need to be interleaved, we
-- need to shift left the bits left by 1 and complement the
-- bit. During decoding, we need to do the reverse, i.e. complement
-- the bit and shift right by 1 with sign extension.
--
-- For the +90 case while encoding we treat the bit stream as all 1's.
-- While decoding we will never get an angle of 90 but can be pretty
-- close.

-- | Adjust the latitude for encoding.
adjustEncodeLat :: Latitude -> Angle
adjustEncodeLat lt
  | testBit lt 63 = clearBit a 63          -- negative angle (starting bit = 0)
  | testBit a 63  = complement zeroBits    -- +90
  | otherwise     = setBit a 63            -- positive angle (starting bit = 1)
  where a = unsafeShiftL (unLat lt) 1

-- | Adjust the angle while decoding.
adjustDecodeLat :: Angle -> Latitude
adjustDecodeLat a = sgnBit .|. unsafeShiftR lt 1
    where lt     = Latitude $ complementBit a 63
          sgnBit = bit 63 .&. lt

-- | Adjusting longitude while encoding. Just nee
adjustEncodeLon :: Longitude -> Angle
adjustEncodeLon = flip complementBit 63 . unLong

-- | Adjusting longitude while decoding
adjustDecodeLon :: Angle -> Longitude
adjustDecodeLon = Longitude . flip complementBit 63

{-
-- | Generates a 24-byte, base32 encoding of geohash values. This
-- encoding is a little bit lossy; Latitudes lose lower 3-bits and
-- Longitudes loose lower 4-bits of information.
toByteString :: GeoHash -> ByteString
toByteString (GeoHash x y) = unsafeCreate 26 $ c_geohash32 x y
-}

--------------- Interleaved base32 encoding ------

-- | The @interleaveAndMerge (x,y)@ merges 5-bits, 3 from @x@ and 2
-- from @y@ into a word and returns it. An appropriate swap is done so
-- that the next bytes are taken from y and x respectively.
interleaveAndMerge :: (Angle, Angle) -> (Word8, (Angle, Angle))
interleaveAndMerge (x,y) = (w, (yp, xp))
  where xp = rotateL x 3  -- Take the top 3 bits
        yp = rotateL y 2  -- Take the top 2 bits
        wx = fromIntegral $ unAngle xp
        wy = fromIntegral $ unAngle yp
        w  = unsafeShiftL     (wx .&. 4) 2     -- x2 -> w4
             .|. unsafeShiftL (wx .&. 2) 1     -- x1 -> w2
             .|.              (wx .&. 1)       -- x0 -> w0
             .|. unsafeShiftL (wy .&. 2) 2     -- y1 -> w3
             .|. unsafeShiftL (wy .&. 1) 1     -- y0 -> w1


encode :: Geo -> GeoHash
encode (Geo lt lng)  = GeoHash $ fst $ B.unfoldrN outputLength fld (adjustEncodeLon lng , adjustEncodeLat lt)
  where fld = Just . interleaveAndMerge

-------------------------- Decoding --------------------------------

-- | This function distributes the bits of the Word8 argument
-- (actually only 5-bits matter) to x and y in an interleaved fashion.
-- x gets 3-bits and y gets 2. The arguments are switched so that for
-- the next byte is distributed to y and x respectively.
splitAndDistribute :: (Angle, Angle) -> Word8 -> (Angle , Angle)
splitAndDistribute (x,y) w = (yp,xp)
  where xp        = unsafeShiftL x 3
                    .|. (4 `bitTo` 2)
                    .|. (2 `bitTo` 1)
                    .|. (0 `bitTo` 0)
        yp        = unsafeShiftL y 2
                    .|. (3 `bitTo` 1)
                    .|. (1 `bitTo` 0)
        bitTo i j = Angle $ fromIntegral $ unsafeShiftL (unsafeShiftR w i .&. 1) j



decode :: GeoHash -> Geo
decode (GeoHash hsh) = Geo lt ln
  where lt     = adjustDecodeLat $ unsafeShiftL y 4
        ln     = adjustDecodeLon $ unsafeShiftL x 4
        (x,y)  = B.foldl splitAndDistribute (Angle 0,Angle 0) strP
        hshLen = B.length hsh
        strP   = if hshLen > outputLength then B.take outputLength hsh
                 else hsh <> B.replicate (outputLength - hshLen) 0
