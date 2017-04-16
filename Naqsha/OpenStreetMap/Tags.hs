{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module exposes some standard tags use in the Open Street Map
-- project. We give a lens based interface for setting and reading tags
-- of open street map elements. The lenses defined here gives a more
-- type safe to set and access the tags than directly using `tagAt`.
--
-- TODO: There are a huge number of standard tags supported by Open
-- Street Map that are not yet present here. Please contribute towards
-- expanding the list here.

module Naqsha.OpenStreetMap.Tags
       (
         name, elevation
       -- * Multi-lingual names.
       -- $multilingual$
       , nameIn
       , module Naqsha.OpenStreetMap.Language
       ) where

import Control.Lens
import Data.Monoid
import Data.Text                            ( Text     )

import Naqsha.Common
import Naqsha.OpenStreetMap.Element
import Naqsha.OpenStreetMap.Language


-- | Low-level lens creator which parses the text tag.
fromTagLens :: (Show a, Read a)
            => Lens' e (Maybe Text)
            -> Lens' e (Maybe a)
fromTagLens lenz = lenz . lens toA fromA
  where toA   ma = ma >>= readMaybeT
        fromA _  = fmap showT


-- | Lens to focus on the name of the element
name :: OsmTagged e => Lens' e (Maybe Text)
name = tagAt "name"

-- | Lens to focus on the elevation (in meters).
elevation :: OsmTagged e => Lens' e (Maybe Double)
elevation = fromTagLens $ tagAt $ "ele"

----------------------- Multi-lingual names -----------------

-- $multilingual$
--
-- Open street map has ways to provide multi-lingual names to objects.
-- Using the `nameIn` lens to set or access the multi-lingual names.

-- | Lens to focus on the name in a given language.
nameIn :: OsmTagged e => Language -> Lens' e (Maybe Text)
nameIn (Language l) = tagAt $ "name:" <> l
