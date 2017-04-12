-- The prelude to use functions used by Naqsha. Only for internal use.

{-# LANGUAGE OverloadedStrings #-}
module Naqsha.Common
       ( showT, readMaybeT, showVersionT, naqshaVersionT
       , showTime, timeParser, osmXmlVersion, buildM, build, withChanges, withChangesM
       ) where

import Control.Monad.State
import Data.Default
import Data.Monoid
import Data.Text       ( Text, pack, unpack       )
import Data.Time
import Data.Version    ( Version(..), showVersion )
import Text.Read       ( readMaybe                )
import Paths_naqsha as NP


-- | Time format used by osm.
osmDisplayFmt :: String
osmDisplayFmt = "%Y-%m-%dT%T%Q%z"

-- | Time parse format (more liberal than display format).
osmParseFmt :: String
osmParseFmt = "%Y-%m-%dT%T%Q%Z"


-- | The xml version of osm.
osmXmlVersion   :: Version
osmXmlVersion   = Version [0,6] []

-- | show the time as text
showTime :: FormatTime t => t -> Text
showTime = pack . formatTime defaultTimeLocale osmDisplayFmt

-- | read teh time as text
timeParser :: (Monad m, ParseTime t) => Text -> m t
timeParser = parseTimeM True defaultTimeLocale osmParseFmt . unpack


-- | Text variants of show
showT :: Show a => a -> Text
showT = pack . show

-- | Text variant of read.
readMaybeT :: Read a => Text -> Maybe a
readMaybeT = readMaybe . unpack


-- | Text variant of show version
showVersionT :: Version -> Text
showVersionT = pack . showVersion


naqshaVersionT :: Text
naqshaVersionT = "naqsha-" <> showVersionT NP.version


-- | Building an element given a set field assignment.
buildM :: (Monad m, Default d) => StateT d m () -> m d
buildM = withChangesM def

-- | A pure version of `buildM`.
build :: Default d => State d () -> d
build = withChanges def

-- | Changes to an element.
withChangesM :: Monad m => d -> StateT d m () -> m d
withChangesM = flip execStateT

-- | A pure version of `withChangesM`.
withChanges :: d -> State d () -> d
withChanges = flip execState
