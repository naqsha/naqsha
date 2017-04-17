{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Naqsha.Arbitrary where

import           Control.Lens
import           Control.Monad.State
import           Data.Default

import           Test.QuickCheck


import Naqsha.Position


instance Arbitrary Angle where
  arbitrary = toEnum <$> arbitrary

instance Arbitrary Latitude where
  arbitrary = lat <$> arbitrary

instance Arbitrary Longitude where
  arbitrary = lon <$> arbitrary


instance Arbitrary Geo where
  arbitrary = Geo <$> arbitrary <*> arbitrary

instance Arbitrary GeoBounds where
  arbitrary = toGen def $ do setArbitrary maxLatitude
                             setArbitrary maxLongitude
                             setArbitrary minLatitude
                             setArbitrary minLongitude
