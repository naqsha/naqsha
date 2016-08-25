{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE OverloadedStrings         #-}
-- | This module captures features of objects.
module Naksha.Feature
       (
         module Naksha.Feature.Internal
         -- ** Supported Features
       , Name(..)
       , Elevation(..)
         -- ** Feature constructor.
       , name, nameIn
       , elevation
       ) where


import qualified Data.Map                   as M
import           Data.Semigroup
import           Data.Text     as T
import           Data.Typeable         ( Typeable )


import Naksha.Feature.Internal

---------  Names of object -------------------------------------------

-- | The language code use to distinguish names in different languages.
type Language = Text

-- | A name of an object. Objects often have multiple names in different languages. In such case,
-- there is a canonical name as well its names in various languages.
data Name = Name { canonicalName :: Text                 -- ^ The canonical name of the object
                 , otherNames    :: M.Map Language Text  -- ^ other (multi-lingual) names.
                 } deriving (Typeable, Show)

-- | Construct the feature of type `Name`.
name  :: Text -> Feature
name  = Feature . flip Name M.empty

-- | Construct the feature of type `Name` having possibly many names
-- in different languages.
nameIn       :: Text -> Text -> Feature
nameIn lang  =  Feature . Name "" . M.singleton lang

instance Semigroup Name where
  (<>) n1 n2
    | T.null $ canonicalName n2 = n1 { otherNames = otherNames n1 `M.union` otherNames n2 }
    | otherwise               = n2 { otherNames = otherNames n1 `M.union` otherNames n2 }

----------------------- Elevation from sea level ------------------------------------


-- | Elevation form mean sea level in metres.
newtype Elevation = Elevation Double deriving (Typeable, Show)

-- | The constructor for the elevation feature.
elevation :: Double -> Feature
elevation = Feature . Elevation


instance Semigroup Elevation where
  (<>) _ e = e
