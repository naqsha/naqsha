{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types          #-}
module Naqsha.Geometry.Coordinate.GeoHashSpec where
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck

import Naqsha.Geometry.Coordinate.GeoHash as GeoHash
import Naqsha.Arbitrary



preserves :: (Eq a, Show a)
          => String
          -> (forall b. Ord b => b -> b -> a)
          -> Spec
preserves name ordF  =
  prop mesg $ \ (g1 :: GeoHash) g2  -> ordF g1 g2 `shouldBe` ordF (toByteString g1) (toByteString g2)
  where mesg = "preserves " ++ name


spec :: Spec
spec = do
  prop "decode . encode should be id" $ forAll geoHashRange $ \ g -> GeoHash.decode (GeoHash.encode g) `shouldBe` g

  describe "toByteString" $ do
    preserves  "compare" compare
    preserves  "<="  (<=)
    preserves  "<"   (<)
    preserves  ">="  (>=)
