module Naqsha.Geometry.Coordinate.GeoHash
       ( GeoHash, encode, decode
       ) where


import           Data.Bits
import qualified Data.ByteString as B
import           Data.Char                ( ord, chr      )
import           Data.String
import           Data.Monoid              ( (<>)          )
import           Data.Word                ( Word64, Word8 )
import           Data.Int                 ( Int64         )


import Naqsha.Geometry.Internal
import Naqsha.Geometry.Coordinate ( Geo(..) )


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
interleaveAndMerge :: (Word64, Word64) -> (Word8, (Word64, Word64))
interleaveAndMerge (x,y) = (w, (yp, xp))
  where xp = rotateL x 3  -- Take the top 3 bits
        yp = rotateL y 2  -- Take the top 2 bits
        wx = fromIntegral xp
        wy = fromIntegral yp
        w  = shiftL     (wx .&. 4) 2     -- x2 -> w4
             .|. shiftL (wx .&. 2) 1     -- x1 -> w2
             .|.        (wx .&. 1)       -- x0 -> w0
             .|. shiftL (wy .&. 2) 2     -- y1 -> w3
             .|. shiftL (wy .&. 1) 1     -- y0 -> w1


encode :: Geo -> GeoHash
encode (Geo lt lng)  = GeoHash $ fst $ B.unfoldrN 24 fld (x , y)
  where fld = Just . interleaveAndMerge
        x   = invertSign $ unAngle $ unLong lng
        y   = invertSignShiftLeft  $ unAngle    $ unLat lt

-------------------------- Decoding --------------------------------

-- | This function distributes the bits of the Word8 argument
-- (actually only 5-bits matter) to x and y in an interleaved fashion.
-- x gets 3-bits and y gets 2. The arguments are switched so that for
-- the next byte is distributed to y and x respectively.
splitAndDistribute :: (Word64, Word64) -> Word8 -> (Word64 , Word64)
splitAndDistribute (x,y) w = (yp,xp)
  where xp        = shiftL x 3 .|. (4 `bitTo` 2)
                               .|. (2 `bitTo` 1)
                               .|. (0 `bitTo` 0)
        yp        = shiftL y 2 .|. (3 `bitTo` 1)
                               .|. (1 `bitTo` 0)
        bitTo i j = fromIntegral $ shiftL (shiftR w i .&. 1) j




decode :: GeoHash -> Geo
decode (GeoHash hsh) = Geo lt ln
  where lt     = Latitude  $ Angle $ invertSignShiftRight $ shiftL y 4
        ln     = Longitude $ Angle $ invertSign           $ shiftL x 4
        (x,y)  = B.foldl splitAndDistribute (0,0) strP
        hshLen = B.length hsh
        strP   = if hshLen > 24 then B.take 24 hsh
                 else hsh <> B.replicate (24 - hshLen) 0
