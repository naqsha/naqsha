{-# LANGUAGE ScopedTypeVariables #-}
module Naqsha.PositionSpec where
import Data.Monoid
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck

import Naqsha.Position
import Naqsha.Arbitrary ()

inRange :: Testable prop
        => (Angle, Angle) -- ^ Range
        -> String         -- ^ description
        -> ((Angle,Angle) -> prop)
        -> Spec
inRange rng@(mi,mx) descr prop_test = it msg $ forAll pair prop_test
  where pair  = (,) <$> gen <*> gen
        msg   = "in range " ++ show rng ++ ": " ++ descr
        gen   =  toEnum <$> choose (fromEnum mi, fromEnum mx)

isIncreasing :: Ord a => (Angle -> a) -> (Angle, Angle) -> Bool
isIncreasing conv (x , y)
  | x == y    = xA == yA
  | x <  y    = xA <  yA
  | otherwise = xA >  yA
  where xA = conv x
        yA = conv y


isDecreasing :: Ord a => (Angle -> a) -> (Angle, Angle) -> Bool
isDecreasing conv (x, y)
  | x == y    = xA == yA
  | x >  y    = xA <  yA
  | otherwise = xA >  yA
  where xA  = conv x
        yA  = conv y



spec :: Spec
spec = do
  describe "latitudes" $ do
    prop "show have a period of 360 deg" $
      \ (x :: Latitude) -> x <> lat 360 `shouldBe` x

    prop "should lie in [-90,90]" $
      \ (x :: Latitude) -> x >= lat (-90) && x <= lat 90

    inRange (-90,90) "increases monotonically"  $ isIncreasing lat
    inRange (90, 270) "decreases monotonically" $ isDecreasing lat


  describe "longitudes" $ do

    prop "should have a period of 360 deg" $
      \ (x :: Longitude) -> x <> lon 360 `shouldBe` x

    prop "should lie in [-180,180]" $
      \ (x :: Longitude) -> x >= lon (-180) && x <= lon 180

    inRange (-180, 180) "increases monotonically" $ isIncreasing lon
