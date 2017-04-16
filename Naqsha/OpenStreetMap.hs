-- | This is the top level module that needs to be imported to work
-- with open street map. We expose the basic elements, tags and meta
-- information from here. We also expose a streaming interface for
-- elements form this module.
--
module Naqsha.OpenStreetMap
       ( module Naqsha.OpenStreetMap.Element
       , module Naqsha.OpenStreetMap.Stream
       , module Naqsha.OpenStreetMap.Tags
       , module Naqsha.Position
       ) where

import Naqsha.OpenStreetMap.Element hiding( OsmMeta
                                          , _osmID, _modifiedUser, _modifiedUserID
                                          , _timeStamp, _changeSet, _version, _isVisible
                                          )

import Naqsha.OpenStreetMap.Stream
import Naqsha.OpenStreetMap.Tags
import Naqsha.Position

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
