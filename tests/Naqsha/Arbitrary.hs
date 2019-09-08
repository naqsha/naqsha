{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Naqsha.Arbitrary where

import Control.Applicative ( (<$>), (<*>) )
import Test.QuickCheck

import Naqsha.Geometry.Angle.Internal
import Naqsha.Geometry.GeoHash
import Naqsha.Geometry.LatLon.Internal



instance Arbitrary Angle where
  arbitrary = toEnum <$> arbitrary

instance Arbitrary Latitude where
  arbitrary = lat <$> arbitrary

instance Arbitrary Longitude where
  arbitrary = lon <$> arbitrary


instance Arbitrary LatLon where
  arbitrary = LatLon <$> arbitrary <*> arbitrary


geoHashRange :: Gen LatLon
geoHashRange = suchThat arbitrary ( \ g -> g /= northPole && g /= southPole)

instance Arbitrary GeoHash where
  arbitrary = encode <$> geoHashRange
