{-# LANGUAGE ScopedTypeVariables #-}
module Naqsha.Geometry.Coordinate.GeoHashSpec where
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck

import Naqsha.Geometry.Coordinate
import Naqsha.Geometry.Coordinate.GeoHash as GeoHash
import Naqsha.Arbitrary ()



geoHashRange :: Gen Geo
geoHashRange = suchThat arbitrary ( \ g -> g /= northPole && g /= southPole)


encodeOrderSpec :: Spec
encodeOrderSpec = prop "preserves ordering under bytstring encoding" $
                  forAll arbitraryPair $ \ (g1,g2)
                                         -> compare g1 g2 `shouldBe` compare (toByteString g1) (toByteString g2)
  where arbitraryGeoHash = encode <$> geoHashRange
        arbitraryPair   = (,) <$> arbitraryGeoHash <*> arbitraryGeoHash


spec :: Spec
spec = describe "decode-encode laws" $ do
  prop "decode . encode should be id" $ forAll geoHashRange $ \ g -> GeoHash.decode (GeoHash.encode g) `shouldBe` g
  encodeOrderSpec
