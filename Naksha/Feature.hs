{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module captures features of objects.
module Naksha.Feature
       ( Name(..), I8NNames, nameIn, nameInLanguage
       , Description(..), Comment(..)
       , Elevation(..)
       , LinkSet(..), link
       ) where

import qualified Data.Map                   as M
import           Data.Semigroup
import           Data.String
import           Data.Text                  as T
import           Data.Typeable         ( Typeable )


import           Naksha.Language
import           Naksha.Annotate

---------  Names of object -------------------------------------------

newtype Name = Name {name :: Text} deriving (Typeable, Show)


instance Semigroup Name where
  (<>) _ n = n

-- | The set of multi-lingual names.
newtype I8NNames = I8NNames { i8nNames :: M.Map Language Text } deriving (Typeable, Show)

-- | Construct the feature of type `Name` having possibly many names
-- in different languages.
nameIn       :: Language
             -> Text
             -> I8NNames
nameIn lang  =  I8NNames . M.singleton lang

nameInLanguage :: Language -> Annotated a -> Maybe Text
nameInLanguage lang a = (a .> i8nNames) >>= M.lookup lang

instance Semigroup I8NNames where
  (<>) n1 n2 = I8NNames $ i8nNames n1 `M.union` i8nNames n2


----------------------- Elevation from sea level ------------------------------------


-- | Elevation form mean sea level in metres.
newtype Elevation = Elevation { elevation :: Double } deriving (Typeable, Show)

instance Semigroup Elevation where
  (<>) _ e = e

-------------------------- Links -------------------------------------------------------

-- | A set of links.
newtype LinkSet = LinkSet { linkSet :: [Text] } deriving (Typeable, Show, Semigroup)

instance IsString LinkSet where
  fromString = LinkSet . (:[]) . fromString

-- | A single link.
link :: Text -> LinkSet
link  = LinkSet . (:[])

------------------------- Description and --------- -------------------------------------

-- | The description of an element.
newtype Description = Description { description :: Text } deriving (Typeable, Show, IsString)

instance Semigroup Description where
  (<>) _ e = e

-------------------------- Comment -------------------------------------------------------

-- | The description of an element.
newtype Comment = Comment { comment :: Text } deriving (Typeable, Show, IsString)

instance Semigroup Comment where
  (<>) _ e = e
