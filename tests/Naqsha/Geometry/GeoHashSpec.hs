{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types          #-}
module Naqsha.Geometry.GeoHashSpec where

import Naqsha.Prelude
import Data.String

import Test.Hspec
import Test.Hspec.QuickCheck

import Naqsha.Geometry
import Naqsha.Geometry.LatLon.Internal
import Naqsha.Instances()

approxEq :: LatLon -> LatLon -> Bool
approxEq (LatLon x1 y1) (LatLon x2 y2) = abs dx <= err && abs dy <= err
  where dx = fromEnum $ toAngle x1 <> invert (toAngle x2)
        dy = fromEnum $ toAngle y1 <> invert (toAngle y2)
        err = bit $ 64 - accuracy

spec :: Spec
spec = do
  prop "fromString . show = id"          $ \ (g :: GeoHash) -> fromString  (show g) `shouldBe` g
  prop "encode . decode  = id"           $ \ (g :: GeoHash) -> encode (decode g)    `shouldBe` g
  prop "decode . encode  = id (approx)"  $ \ (g :: LatLon)     -> decode (encode g)    `approxEq` g
