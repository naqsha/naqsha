-- | Naqsha library version.
module Naqsha.Version
       ( version
       , versionString
       ) where

import           Data.Version
import qualified Paths_naqsha as NP

-- | The version string associated with naqsha.
versionString :: String
versionString = "naqsha-" ++ showVersion version

-- | The naqsha library version
version :: Version
version = NP.version
