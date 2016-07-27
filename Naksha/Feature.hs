{-# LANGUAGE DeriveDataTypeable        #-}
-- | This module captures features of objects.
module Naksha.Feature
       ( -- * Features and FeatureSet
         -- $featureset$
         module Naksha.Feature.Internal
         -- ** Supported features.
       , module Naksha.Feature.Name
       ) where


import Naksha.Feature.Internal
import Naksha.Feature.Name

-- $featureset$
--
-- Maps in naksha consists of objects. A feature, captured by the type
-- `Feature`, are properties that determine properties of an
-- object. Objects in naksha can have pretty generic be assigned any
-- features (any instance of `Typeable` is allowed). However, depending
-- on the actual mapping format, some features are meaningless.
--
-- The type `FeatureSet` captures a set of features. It allows for
-- extraction of individual features if it exists.
--
