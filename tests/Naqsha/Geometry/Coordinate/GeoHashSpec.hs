{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types          #-}
module Naqsha.Geometry.Coordinate.GeoHashSpec where
import Data.String
import Test.Hspec
import Test.Hspec.QuickCheck

import Naqsha.Geometry.Coordinate.GeoHash as GeoHash
import Naqsha.Arbitrary()



spec :: Spec
spec = do
  prop "fromString . show = id" $ \ (g :: GeoHash) -> (fromString  $ show g) `shouldBe` g
  prop "encode . decode  = id"  $ \ (g :: GeoHash) -> (encode $ decode g)    `shouldBe` g
