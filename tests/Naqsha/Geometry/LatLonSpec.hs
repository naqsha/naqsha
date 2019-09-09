{-# LANGUAGE ScopedTypeVariables #-}
module Naqsha.Geometry.LatLonSpec where

import Control.Applicative ( (<$>), (<*>) )
import Data.Monoid
import Data.Fixed
import Data.Group
import Test.QuickCheck        as QC
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.SmallCheck  as SC
import Test.SmallCheck        as SC
import Test.SmallCheck.Series as SC


import Naqsha.Geometry
import Naqsha.Geometry.Angle.Internal
import Naqsha.Instances ()

inRange :: (Angle, Angle) -- ^ Range
        -> String         -- ^ description
        -> ((Angle,Angle) -> Bool)
        -> Spec
inRange (mi,mx) descr prop_test = it msg $ QC.forAll pair prop_test
  where pair  = (,) <$> gen <*> gen
        msg   = "in range " ++ show (toNano mi,toNano mx) ++ ": " ++ descr
        gen   =  toEnum <$> choose (fromEnum mi, fromEnum mx)

        toNano :: Angle -> Nano
        toNano = toDegree

-- | The epsilon neighbour Hood.
epsilon :: Monad m => Series m Angle
epsilon = Angle .  SC.getNonNegative <$>  limit 256 series

getLatAngle :: LatLon -> Angle
getLatAngle (LatLon x _) = unLat x

minAtSouthPole :: Spec
minAtSouthPole = it "have a minima at south pole"
                 $ SC.property
                 $ over epsilon
                 $ \ eps
                   -> isIncreasing (ang, ang <> eps) &&
                      isDecreasing (ang <> invert eps, ang)
  where ang = getLatAngle southPole

maxAtNorthPole :: Spec
maxAtNorthPole = it "have a maxima at north pole"
                 $ SC.property
                 $ over epsilon
                 $ \ eps
                   -> isDecreasing (ang, ang <> eps) &&
                      isIncreasing (ang <> invert eps, ang)
  where ang = getLatAngle northPole

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

    minAtSouthPole
    maxAtNorthPole

    inRange (degree (-90),  degree 90)    "increases monotonically" isIncreasing
    inRange (degree 90 ,  maxBound)     "decreases monotonically"   isDecreasing
    inRange (minBound , degree (-90)) "decreases monotonically"     isDecreasing

    shouldBeBounded (undefined :: Latitude)

  describe "longitudes" $ do

    prop "should have a period of 360 deg" $
      \ (x :: Longitude) -> x <> lon (degree 360) `shouldBe` x

    shouldBeBounded (undefined ::Longitude)
