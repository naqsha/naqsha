{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE Rank2Types                 #-}

-- | This module captures exposes some common features supported by us.
module Naqsha.Annotate.Feature where


import           Control.Lens
import           Data.Map                  as Map
import           Data.String
import           Data.Semigroup
import           Data.Text                  as T
import           Data.Typeable         ( Typeable )
import           Naqsha.Language


----------------------- Elevation from sea level ------------------------------------

-- | The name feature.
newtype Name = Name { _name :: Text }  deriving (Typeable, Show, IsString)
makeLenses ''Name

-- | Multi-lingual names.
newtype MultiName = MultiName { _multiNames :: Map Language Text } deriving (Typeable, Show, Monoid)

makeLenses ''MultiName


-- | Elevation form mean sea level in metres.
newtype Elevation = Elevation { _elevation :: Double } deriving (Typeable, Show, Num, Fractional)
makeLenses ''Elevation



-------------------------- Links -------------------------------------------------------

-- | A set of links.
newtype LinkSet = LinkSet { _linkSet :: [Text] } deriving (Typeable, Show, Semigroup)

makeLenses ''LinkSet

instance IsString LinkSet where
  fromString = LinkSet . (:[]) . fromString

------------------------- Description and --------- -------------------------------------

-- | The description of an element.
newtype Description = Description { _description :: Text} deriving (Typeable, Show, IsString)
makeLenses ''Description

-------------------------- Comment -------------------------------------------------------

-- | The description of an element.
newtype Comment = Comment { _comment :: Text } deriving (Typeable, Show, IsString)
makeLenses ''Comment
