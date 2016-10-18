{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE Rank2Types                 #-}

-- | This module captures features of objects.
module Naksha.Feature where


import           Control.Lens
import           Data.Map                  as Map
import           Data.String
import           Data.Semigroup
import           Data.Text                  as T
import           Data.Typeable         ( Typeable )

import           Naksha.Language



----------------------- Elevation from sea level ------------------------------------

newtype Name = Name { _name :: Text }  deriving (Typeable, Show, IsString)

makeLenses ''Name


-- | Multi-lingual name.
newtype MultiName = MultiName { _multiNames :: Map Language Text } deriving (Typeable, Show, Monoid)


--
makeLenses ''MultiName

-- | Elevation form mean sea level in metres.
newtype Elevation = Elevation { _elevation :: Double } deriving (Typeable, Show, Num, Fractional)

makeLenses ''Elevation


-------------------------- Links -------------------------------------------------------

-- | A set of links.
newtype LinkSet = LinkSet { linkSet :: [Text] } deriving (Typeable, Show, Semigroup)

instance IsString LinkSet where
  fromString = LinkSet . (:[]) . fromString

------------------------- Description and --------- -------------------------------------

-- | The description of an element.
newtype Description = Description Text deriving (Typeable, Show, IsString)

-------------------------- Comment -------------------------------------------------------

-- | The description of an element.
newtype Comment = Comment Text deriving (Typeable, Show, IsString)
