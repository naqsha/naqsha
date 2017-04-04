module Naqsha.OpenStreetMap
       ( module Naqsha.OpenStreetMap.Element
       , module Naqsha.OpenStreetMap.Language
       , module Naqsha.OpenStreetMap.Tags
       ) where

import Naqsha.OpenStreetMap.Element hiding( OsmMeta
                                          , _osmID, _modifiedUser, _modifiedUserID
                                          , _timeStamp, _changeSet, _version, _isVisible
                                          , osmID, modifiedUser, modifiedUserID
                                          , timeStamp, changeSet, version, isVisible
                                          )

import Naqsha.OpenStreetMap.Language hiding ( Language, lang )
import Naqsha.OpenStreetMap.Tags
