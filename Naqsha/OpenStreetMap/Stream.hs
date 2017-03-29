-- | This module exposes a streaming interface for open street map elements.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE FlexibleInstances #-}
module Naqsha.OpenStreetMap.Stream
       (
         -- * An Open Street map event type
         OsmEvent(..), OsmSource
       , OsmEventElement(..)
       ) where

import Control.Lens                 ( (^.) )
import Data.Conduit
import Data.Conduit.Combinators     ( yieldMany )
import Data.HashMap.Lazy         as HM
import Data.Monoid                  ( (<>) )
import Data.Text                    (Text)


import Naqsha.Position
import Naqsha.OpenStreetMap.Element


------------------------ Osm events --------------------------

-- | Osm events for streaming.
data OsmEvent = EventGeoBounds GeoBounds
              | EventMember    Member
              | EventTag       Text  Text
              | EventNodeRef   NodeID
              ------------------------- Nested events ---------------------------------------
              | EventBeginOsm
              | EventEndOsm
              | EventNodeBegin Node (OsmMeta Node)
              | EventNodeEnd
              | EventWayBegin      (OsmMeta Way)
              | EventWayEnd
              | EventRelationBegin (OsmMeta Relation)
              | EventRelationEnd deriving (Show, Eq)


-------------- Some combinators for building sources ---------------

-- | Build a stream with a single node element.
node :: Monad m
     => Node         -- ^ The node
     -> OsmMeta Node -- ^ The metadata
     -> OsmSource m  -- ^ The body (typically just the tags)
     -> OsmSource m
node n mt os = yield (EventNodeBegin n mt) >> os >> yield EventNodeEnd


-- | Build a stream with a single way.
way :: Monad m
   => OsmMeta Way -- ^ The meta data
   -> OsmSource m -- ^ The body.
   -> OsmSource m
way mt os = yield (EventWayBegin mt) >> os >> yield EventWayEnd

-- | Build a relation stream.
relation :: Monad m
         => OsmMeta Relation -- ^ The meta data
         -> OsmSource m      -- ^ The body
         -> OsmSource m
relation mt os = yield (EventRelationBegin mt) >> os >> yield EventRelationEnd

-- | Osm event Source
type OsmSource m = Source m OsmEvent

-- | Types that an be converted to a source of osmEvents.
class OsmEventElement a where
  -- | Given an element convert it into a source.
  toSource :: Monad m => a -> OsmSource m

------------------- Nested events --------------------------------------
instance OsmEventElement (Osm Node) where
  toSource og = node (og ^. untagged) (og ^. meta) body
    where body = tagsToSource (og ^. tags)

instance OsmEventElement (Osm Way) where
  toSource wy = way (wy ^. meta) body
    where body     = nodeRefs <> tagsToSource (wy ^. tags)
          nodeRefs = mapOutput EventNodeRef $ yieldMany (wy ^. untagged . wayNodes)

instance OsmEventElement (Osm Relation) where
  toSource rel = relation  (rel ^. meta) body
    where body = memSrc <> tagsToSource (rel ^. tags)
          memSrc   = mapOutput EventMember $ yieldMany (rel ^. untagged . relationMembers)

instance OsmEventElement Member where
  toSource  = yield . EventMember
instance OsmEventElement GeoBounds where
  toSource = yield . EventGeoBounds

---------------------------- Unnested events. --------------------------

-- | Convert tags to source.
tagsToSource :: Monad m => OsmTags -> OsmSource m
tagsToSource = HM.foldrWithKey tgFunc mempty
  where tgFunc k v  = mappend $ yield $ EventTag k v
