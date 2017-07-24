{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Naqsha.Arbitrary where

import Control.Applicative ( (<$>) )
import Test.QuickCheck


import Naqsha.Geometry
import Naqsha.Geometry.Coordinate.GeoHash


instance Arbitrary Angle where
  arbitrary = toEnum <$> arbitrary

instance Arbitrary Latitude where
  arbitrary = lat <$> arbitrary

instance Arbitrary Longitude where
  arbitrary = lon <$> arbitrary


instance Arbitrary Geo where
  arbitrary = Geo <$> arbitrary <*> arbitrary


geoHashRange :: Gen Geo
geoHashRange = suchThat arbitrary ( \ g -> g /= northPole && g /= southPole)

instance Arbitrary GeoHash where
  arbitrary = encode <$> geoHashRange
