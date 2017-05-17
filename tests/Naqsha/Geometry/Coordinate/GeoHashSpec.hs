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

spec :: Spec
spec = describe "decode-encode laws" $ do
  prop "decode . encode should be id" $ forAll geoHashRange $ \ g -> GeoHash.decode (GeoHash.encode g) `shouldBe` g
