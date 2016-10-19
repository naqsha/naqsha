{-# LANGUAGE Rank2Types                 #-}

-- | This module exposes the annotation attributes.
module Naqsha.Annotate.Attribute where

import           Control.Lens
import           Data.Map                  as Map
import           Data.Text                 as T
import           Naqsha.Language

import qualified Naqsha.Annotate.Feature  as F
import           Naqsha.Annotate.Internal

-- | The name attribute.
name :: Attribute a Text
name = fromIso F.name

-- | Name of the object in the given language.
nameIn ::  Language -> Attribute a Text
nameIn lang = feature . _fromMaybe mempty . F.multiNames . at lang


-- | Attribute capturing the list of language specific names of the object.
multiNames :: Attribute a [(Language, Text)]
multiNames = fromIso $ F.multiNames . iso Map.toList Map.fromList


-- | The set of links associated with the object.
links :: Attribute a [Text]
links = fromIso F.linkSet


-- | The elevation (in metres) from the sea level.
elevation :: Attribute a Double
elevation = fromIso F.elevation
