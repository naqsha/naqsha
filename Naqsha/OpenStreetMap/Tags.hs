{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module exposes some standard tags use in the open street
-- map. We give a lens based interface for setting and reading tags of
-- open street map elements. The lenses defined here gives a more type
-- safe to set and access the tags than directly using `tagAt`.
--
-- TODO: There are a huge number of standard tags supported by Open
-- Street Map that are not yet present here. Please contribute towards
-- expanding the list here.
module Naqsha.OpenStreetMap.Tags
       ( name, nameIn, elevation
       ) where

import Control.Lens
import Data.Monoid
import Data.Text                    (Text)
import Naqsha.Common
import Naqsha.OpenStreetMap.Element
import Naqsha.OpenStreetMap.Language


fromTagLens :: (Show a, Read a)
            => Lens' e (Maybe Text)
            -> Lens' e (Maybe a)
fromTagLens lenz = lenz . lens toA fromA
  where toA   ma  = ma >>= readMaybeT
        fromA _ a = Just $ showT a

-- | Lens to focus on the name of the element
name :: OsmTagged e => Lens' e (Maybe Text)
name = tagAt "name"

-- | Lens to focus on the name in a given language.
nameIn :: OsmTagged e => Language -> Lens' e (Maybe Text)
nameIn (Language l) = tagAt $ "name:" <> l

-- | Lens to focus on the elevation (in meters).
elevation :: OsmTagged e => Lens' e (Maybe Double)
elevation = fromTagLens $ tagAt $ "ele"
