-- | This module exposes a streaming interface for open street map elements.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE FlexibleInstances #-}
module Naqsha.OpenStreetMap.Stream
       (
         -- * Streaming interface.
         -- $osmstream$
         OsmSource, OsmEvent(..)
       , OsmEventElement(..), toSource
       , osmFile
       ) where

import Control.Lens                 ( (^.) )
import Data.Conduit
import Data.Conduit.Combinators     ( yieldMany )
import Data.Default                 ( def       )
import Data.HashMap.Lazy         as HM
import Data.Monoid                  ( (<>) )
import Data.Text                    (Text)


import Naqsha.Position
import Naqsha.OpenStreetMap.Element

-- $osmstream$
--
-- Open Street Map elements are streamed by streaming values of type
-- `OsmEvent`. An important streaming operation is to convert a given
-- Open Street Map element into the corresponding stream. This is
-- accomplished using the combinator `toSource`. For any type that is
-- an instance of the class `OsmEventElement` can be converted to a
-- source of `OsmEvent` using this combinator.  Finally the combinator
-- `osmFile` can be used to put together a complete osm file.
--
-- Here is an example containing a single node.
--
--
-- > myMap :: OsmSource m
-- > myMap = osmFile region $ toSource taj
-- >     where region = build $ do maxLatitude  .= lat 27.17396
-- >                               minLatitude  .= lat 27.17394
-- >                               maxLongitude .= lon 78.04430
-- >                               minLongitude .= lon 78.04428
-- >           taj    :: Tagged Node
-- >           taj    =  build $ do latitude     .= lat 27.17395
-- >                                longitude    .= lon 78.04429
-- >                                name         .= Just "Taj Mahal"
-- >
--
--
-- The total number of elements that describe a certain portion of a
-- map are often huge. Therefore the only feasible method to process
-- non-trivial regions of the world is by building a streaming
-- interface.
------------------------ Osm events --------------------------

-- | Osm events for streaming.
data OsmEvent = EventGeoBounds GeoBounds
              | EventMember    Member
              | EventTag       Text  Text
              | EventNodeRef   NodeID
              ------------------------- Nested events ---------------------------------------
              | EventBeginOsm
              | EventEndOsm
              | EventNodeBegin (OsmMeta Node) Node
              | EventNodeEnd
              | EventWayBegin  (OsmMeta Way)
              | EventWayEnd
              | EventRelationBegin (OsmMeta Relation)
              | EventRelationEnd deriving (Show, Eq)


-------------- Some combinators for building sources ---------------

-- | Stream an osm file given its bounds and contents. The contents
-- stream should only generated events corresponding to `Node`s,
-- `Way`s and `Relation`s. Otherwise the resulting file will not be
-- wellformed.
osmFile :: Monad m
        => GeoBounds   -- ^ The boundary associated with the file
        -> OsmSource m -- ^ The contents
        -> OsmSource m
osmFile gb contents = yield EventBeginOsm
                      <> yield (EventGeoBounds gb)
                      <> contents
                      <> yield EventEndOsm

-- | Osm event Source
type OsmSource m = Source m OsmEvent

-- | Types that an be converted to a source of osmEvents.
class OsmEventElement a where

  -- | The starting event of the element.
  startEvent :: a -> OsmEvent

  -- | The body source for the given element.
  bodySource :: Monad m => a -> OsmSource m

  -- | The ending event of the element.
  endEvent   :: a -> OsmEvent


-- | Given an event element convert it into a source.
toSource :: (OsmEventElement a, Monad m)
         => a
         -> OsmSource m
toSource a = yield (startEvent a) <> bodySource a <> yield (endEvent a)


------------------- Nested events --------------------------------------
instance OsmEventElement Node where
  startEvent = EventNodeBegin def
  endEvent   = const EventNodeEnd
  bodySource = mempty


instance OsmEventElement Way where
  startEvent   = const $ EventWayBegin def
  endEvent     = const EventWayEnd
  bodySource w = mapOutput EventNodeRef $ yieldMany (w ^. wayNodes)


instance OsmEventElement Relation where
  startEvent   = const $ EventRelationBegin def
  endEvent     = const EventRelationEnd
  bodySource r = mapOutput EventMember $ yieldMany (r ^. relationMembers)

instance OsmEventElement e => OsmEventElement (Tagged e) where
  startEvent te = startEvent (te ^. untagged)
  endEvent   te = endEvent   (te ^. untagged)
  bodySource te = tagsToSource (te ^. tags) <> bodySource (te ^. untagged)

instance OsmEventElement (Osm Node) where
  startEvent og = EventNodeBegin (og ^. meta) (og ^. untagged)
  endEvent      = const EventNodeEnd
  bodySource og = bodySource (og ^. unMeta)

instance OsmEventElement (Osm Way) where
  startEvent ow = EventWayBegin (ow ^. meta)
  endEvent      = const EventWayEnd
  bodySource ow = bodySource (ow ^. unMeta)

instance OsmEventElement (Osm Relation) where
  startEvent orel  = EventRelationBegin (orel ^. meta)
  endEvent         = const EventRelationEnd
  bodySource orel  = bodySource (orel ^. unMeta)

---------------------------- Unnested events. --------------------------

-- | Convert tags to source.
tagsToSource :: Monad m => OsmTags -> OsmSource m
tagsToSource = HM.foldrWithKey tgFunc mempty
  where tgFunc k v  = mappend $ yield $ EventTag k v
