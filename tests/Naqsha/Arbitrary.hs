{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Naqsha.Arbitrary where

import           Control.Lens
import           Control.Monad.Trans ( lift )
import           Test.QuickCheck


import Naqsha


instance Arbitrary Angle where
  arbitrary = toEnum <$> arbitrary

instance Arbitrary Latitude where
  arbitrary = lat <$> arbitrary

instance Arbitrary Longitude where
  arbitrary = lon <$> arbitrary


instance Arbitrary Geo where
  arbitrary = Geo <$> arbitrary <*> arbitrary

instance Arbitrary GeoBounds where
  arbitrary = buildM $ do maxLatitude  <~ lift arbitrary
                          maxLongitude <~ lift arbitrary
                          minLatitude  <~ lift arbitrary
                          minLongitude <~ lift arbitrary
