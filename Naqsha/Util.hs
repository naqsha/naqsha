-- | Lens based utilities.
module Naqsha.Util
       ( -- * Lens utilities for building and changing.
         -- $utils$
         build, withChanges
       , buildM, withChangesM
       ) where


import Control.Monad.State ( execState, execStateT, StateT, State)
import Data.Default

-- $utils$
--
-- Lenses form an important part of naqsha's interface. The
-- combinators `build`, `withChanges`, and their monadic counter parts
-- `buildM`, and `withChangesM` gives easy interface to construct or
-- change typical elements that arise here.

-- | Building an element given a set field assignment. The element
-- should be an instance of `Default` class. These functions are
-- typically used in the context of lenses see for example.
--
-- > kanpur :: Geo
-- > kanpur = build $ do latitude  .= lat 26.4477777
-- >                     longitude .= lon 80.3461111
--
-- `build` is equivalent to @`withChanges` def @.

build :: Default d => State d () -> d
build = withChanges def

-- | A monadic version of `build`.
--
-- > readPosition = IO Geo
-- > readPosition = buildM $ do latitude  <~ readLat
-- >                            longitude <~ readLong
--
-- `buildM` is equivalent to @`withChangesM` def@.
--
buildM :: (Monad m, Default d) => StateT d m () -> m d
buildM = withChangesM def

-- | A monadic version of  `withChanges`.
--
withChangesM :: Monad m => d -> StateT d m () -> m d
withChangesM = flip execStateT


-- | Changes to an element.
--
-- > kanpurChange :: Geo
-- > kanpurChange = kanpur `withChanges` do longitude .= lon 80.346
-- >                                        latitude  .= lat 26.447

withChanges :: d -> State d () -> d
withChanges = flip execState
