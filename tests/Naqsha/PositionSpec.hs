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
    prop "read . show = id" $
      \ (x :: Latitude) -> x `shouldBe` read (show x)

    prop "show have a period of 360 deg" $
      \ (x :: Latitude) -> x <> lat 360 `shouldBe` x

    shouldBeBounded (undefined ::Latitude)

    inRange (-90,90)        "increases monotonically" $ isIncreasing lat
    inRange (90,maxBound)   "decreases monotonically" $ isDecreasing lat
    inRange (minBound, -90) "decreases monotonically" $ isDecreasing lat
  describe "longitudes" $ do

    prop "read . show = id" $
      \ (x :: Latitude) -> x `shouldBe` read (show x)

    prop "should have a period of 360 deg" $
      \ (x :: Longitude) -> x <> lon 360 `shouldBe` x

    shouldBeBounded (undefined ::Longitude)

    inRange (succ (-180), 180) "increases monotonically" $ isIncreasing lon
