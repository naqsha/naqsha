-- The prelude to use functions used by Naqsha. Only for internal use.

{-# LANGUAGE OverloadedStrings #-}
module Naqsha.Common
       ( showT, readT, showVersionT, naqshaVersionT
       , showTime, timeParser, osmXmlVersion
       ) where

import Data.Time
import Data.Monoid
import Data.Text       ( Text, pack, unpack   )
import Data.Version    ( Version(..), showVersion )
import Paths_naqsha as NP


-- | Time format used by osm.
osmTimeFmt :: String
osmTimeFmt = "%Y-%m-%dT%T%Q%z"

-- | The xml version of osm.
osmXmlVersion   :: Version
osmXmlVersion   = Version [0,6] []

-- | show the time as text
showTime :: FormatTime t => t -> Text
showTime = pack . formatTime defaultTimeLocale osmTimeFmt

-- | read teh time as text
timeParser :: (Monad m, ParseTime t) => Text -> m t
timeParser = parseTimeM True defaultTimeLocale osmTimeFmt . unpack


-- | Text variants of show
showT :: Show a => a -> Text
showT = pack . show

-- | Text variant of read.
readT :: Read a => Text -> a
readT = read . unpack


-- | Text variant of show version
showVersionT :: Version -> Text
showVersionT = pack . showVersion


naqshaVersionT :: Text
naqshaVersionT = "naqsha-" <> showVersionT NP.version
