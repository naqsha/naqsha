{-# LANGUAGE ScopedTypeVariables #-}
module Naqsha.Geometry.CoordinateSpec where

import Control.Applicative ( (<$>), (<*>) )
import Data.Monoid
import Data.Fixed
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck

import Naqsha.Geometry
import Naqsha.Arbitrary ()

inRange :: Testable prop
        => (Angle, Angle) -- ^ Range
        -> String         -- ^ description
        -> ((Angle,Angle) -> prop)
        -> Spec
inRange (mi,mx) descr prop_test = it msg $ forAll pair prop_test
  where pair  = (,) <$> gen <*> gen
        msg   = "in range " ++ show (toNano mi,toNano mx) ++ ": " ++ descr
        gen   =  toEnum <$> choose (fromEnum mi, fromEnum mx)

        toNano :: Angle -> Nano
        toNano = toDegree

isIncreasing :: (Angle, Angle) -> Bool
isIncreasing (x , y)
  | x == y    = xA == yA
  | x <  y    = xA <  yA
  | otherwise = xA >  yA
  where xA = lat x
        yA = lat y


isDecreasing :: (Angle, Angle) -> Bool
isDecreasing (x, y)
  | x == y    = xA == yA
  | x >  y    = xA <  yA
  | otherwise = xA >  yA
  where xA  = lat x
        yA  = lat y

shouldBeBounded :: (Arbitrary a, Ord a, Show a, Bounded a) => a -> Spec
shouldBeBounded a = prop msg $ \ x -> x >= mi && x <= mx
  where msg = unwords [ "should lie between"
                      , show mi
                      , "and"
                      , show mx
                      ]
        mi   = minBound `asTypeOf` a
        mx   = maxBound `asTypeOf` a


spec :: Spec
spec = do

  describe "latitudes" $ do

    inRange (degree (-90),  degree 90)    "increases monotonically" isIncreasing
    inRange (degree 90 ,  maxBound)     "decreases monotonically"   isDecreasing
    inRange (minBound , degree (-90)) "decreases monotonically"     isDecreasing

    shouldBeBounded (undefined :: Latitude)

  describe "longitudes" $ do

    prop "should have a period of 360 deg" $
      \ (x :: Longitude) -> x <> lon (degree 360) `shouldBe` x

    shouldBeBounded (undefined ::Longitude)
