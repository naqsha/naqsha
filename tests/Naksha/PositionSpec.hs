module Naksha.PositionSpec where

import Test.Hspec

import Naksha.Position


spec :: Spec
spec = do
  describe "latitudes" $ do
    it "should increase till 90"
      $ deg 90  `shouldBe` (deg 90  :: Latitude)
    it "should decrease after 90"
      $ deg 91  `shouldBe` (deg 89  :: Latitude)
    it "should become zero at 180"
      $ deg 180 `shouldBe` (deg 0  :: Latitude)
    it "should become negative beyond 180"
      $ deg 181   `shouldBe` (deg (-1) :: Latitude)
    it "should decrease till -90"
      $ deg (-90)   `shouldBe` (deg (-90)  :: Latitude)
    it "should increase after -90"
      $ deg (-91)   `shouldBe` (deg (-89)  :: Latitude)
    it "should become zero at -180"
      $ deg (-180) `shouldBe` (deg 0  :: Latitude)
    it "should become positive beyond -180"
      $ deg (-181) `shouldBe` (deg 1 :: Latitude)
