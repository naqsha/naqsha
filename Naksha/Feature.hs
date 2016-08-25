{-# LANGUAGE DeriveDataTypeable        #-}
-- | This module captures features of objects.
module Naksha.Feature
       (
         module Naksha.Feature.Internal
         -- ** Supported Features
       , Name(..)
         -- ** Feature constructor.
       , name, names
       ) where


import           Data.Text
import           Data.Typeable( Typeable )

import Naksha.Feature.Internal

---------  Names of object -------------------------------------------

-- | The language code use to distinguish names in different languages.
type Language = Text

-- | A name of an object. Objects often have multiple names in different languages. In such case,
-- there is a canonical name as well its names in various languages.
data Name = Name { canonicalName :: Text              -- ^ The canonical name of the object
                 , otherNames    :: [(Language,Text)] -- ^ other (multi-lingual) names.
                 } deriving Typeable

-- | Construct the feature of type `Name`.
name  :: Text -> Feature
name  = flip names []

-- | Construct the feature of type `Name` having possibly many names
-- in different languages.
names  :: Text -> [(Language, Text)] -> Feature
names t = Feature . Name t
