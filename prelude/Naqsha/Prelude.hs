{-# LANGUAGE CPP #-}
-- |
--
-- Module      : Naqsha.Prelude
-- Description : A minimal module that acts as prelude for all naqsha packages
-- Copyright   : (c) Piyush P Kurur, 2020
-- License     : Apache-2.0 OR BSD-3-Clause
-- Maintainer  : Piyush P Kurur <ppk@iitpkd.ac.in>
-- Stability   : experimental
--

module Naqsha.Prelude ( module X
                      , module Prelude
                      ) where


import Control.Applicative          as X
import Data.Bits                    as X
import Data.Group                   as X
import Data.Int                     as X
import Data.Maybe                   as X

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid                  as X
import Data.Semigroup               as X
#endif

import Data.Word                    as X
