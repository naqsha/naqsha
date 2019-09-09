{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Naqsha.Instances where

import Control.Applicative ( (<$>), (<*>) )
import Test.QuickCheck
import Test.SmallCheck.Series

import Naqsha.Geometry.Angle.Internal
import Naqsha.Geometry.GeoHash
import Naqsha.Geometry.LatLon.Internal



instance Arbitrary Angle where
  arbitrary = toEnum <$> arbitrary

instance Monad m => Serial m Angle where
  series = Angle <$> series

instance Arbitrary Latitude where
  arbitrary = lat <$> arbitrary

instance Monad m => Serial m Latitude where
  series = lat <$> series

instance Arbitrary Longitude where
  arbitrary = lon <$> arbitrary

instance Monad m => Serial m Longitude where
  series = lon <$> series


instance Arbitrary LatLon where
  arbitrary = LatLon <$> arbitrary <*> arbitrary


geoHashRange :: Gen LatLon
geoHashRange = suchThat arbitrary ( \ g -> g /= northPole && g /= southPole)

instance Arbitrary GeoHash where
  arbitrary = encode <$> geoHashRange
