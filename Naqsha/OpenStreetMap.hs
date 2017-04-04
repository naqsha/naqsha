module Naqsha.OpenStreetMap
       ( module Naqsha.OpenStreetMap.Element
       , module Naqsha.OpenStreetMap.Language
       ) where

import Naqsha.OpenStreetMap.Element hiding( OsmMeta
                                          , _osmID, _modifiedUser, _modifiedUserID
                                          , _timeStamp, _changeSet, _version, _isVisible
                                          )

import Naqsha.OpenStreetMap.Language hiding ( lang )
import Naqsha.OpenStreetMap.Tags
