{-# LANGUAGE ScopedTypeVariables #-}
module Naqsha.PositionSpec where
import Data.Monoid
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck

import Naqsha.Position
import Naqsha.Arbitrary ()

inRange :: Testable prop
        => (Int, Int)    -- ^ Range
        -> String        -- ^ description
        -> ((Int,Int) -> prop)
        -> Spec
inRange rng descr prop = it label $ forAll pair prop
  where pair = (,) <$> choose rng <*> choose rng
        label = "in range " ++ show rng ++ ": " ++ descr


isIncreasing :: Angular a => a -> (Int, Int) ->  Bool
isIncreasing a (x , y) | x == y = toDeg xA == toDeg yA
                       | x <  y = toDeg xA <  toDeg yA
                       | x >  y = toDeg xA >  toDeg yA
  where xA = intToAngular x
        yA = intToAngular y
        intToAngular = flip asTypeOf a . deg . toEnum

isDecreasing :: Angular a => a -> (Int, Int) -> Bool
isDecreasing a (x, y) | x == y = toDeg xA == toDeg yA
                      | x >  y = toDeg xA <  toDeg yA
                      | x <  y = toDeg xA >  toDeg yA
  where xA = intToAngular x
        yA = intToAngular y
        intToAngular = flip asTypeOf a . deg . toEnum


spec :: Spec
spec = do
  describe "latitudes" $ do
    prop "show have a period of 360 deg" $ \ (x :: Latitude) ->
      x <> deg 360 == x

    prop "normalised latitude lie in -90 to 90" $ \ (x :: Latitude) ->
      let xDeg = toDeg (normalise x)
          in xDeg >= -90 && xDeg <= 90

    inRange (-90,90) "latitude increases" $ isIncreasing (undefined :: Latitude)
    inRange (90, 270) "latitude decreases" $ isDecreasing (undefined :: Latitude)


  describe "longitudes" $ do
    prop "should have a period of 360 deg" $ \ (x :: Longitude) ->
      x <> deg 360 == x

    prop "normalised longitude lie in -180 to 180" $ \ (x :: Longitude) ->
      let xDeg = toDeg (normalise x)
          in xDeg >= -180 && xDeg <= 180

    inRange (-180, 180) "longitude increases" $ isIncreasing (undefined :: Longitude)
