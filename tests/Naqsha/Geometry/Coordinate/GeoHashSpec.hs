{-# LANGUAGE ScopedTypeVariables #-}
module Naqsha.Geometry.Coordinate.GeoHashSpec where
import Data.Monoid
import Data.Fixed
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck

import Naqsha.Geometry.Coordinate
import Naqsha.Geometry.Coordinate.GeoHash as GeoHash
import Naqsha.Geometry.Angle
import Naqsha.Arbitrary ()

spec :: Spec
spec = describe "decode-encode laws" $ do
  prop "decode . encode should be id" $ \ g -> GeoHash.decode (GeoHash.encode g) `shouldBe` g
