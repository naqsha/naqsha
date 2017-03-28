{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
module Naqsha.OpenStreetMap.XML
       ( -- * Compiling to XML
         compile, compileDoc
       ) where

import            Control.Lens
import            Control.Monad
import            Control.Monad.State
import            Data.Conduit                ( Conduit, yield, await        )
import            Data.Conduit.List           ( concatMap                    )
import            Data.Default                ( def                          )
import  qualified Data.Map        as M
import            Data.Maybe                  ( catMaybes                    )
import            Data.Text                   ( Text, unpack                 )
import            Data.XML.Types              ( Event(..), Name, Content(..) )
import            Prelude         hiding      ( concatMap                    )
import            Text.XML.Stream.Parse

import Naqsha.Common
import Naqsha.Position
import Naqsha.OpenStreetMap.Element
import Naqsha.OpenStreetMap.Stream

type Compile m = Conduit OsmEvent m Event
type Parse   m = Conduit Event    m OsmEvent

-- | OSM xml name space.
xmlNameSpace :: Text
xmlNameSpace = "http://openstreetmap.org/osm/0.6"

-- | Conduit to convert Osm Events to xml.
compile :: Monad m => Compile m
compile = concatMap compiler

-- | Conduit to convert Osm Events to a complete xml document, i.e. with preamble.
compileDoc :: Monad m => Compile m
compileDoc = do yield EventBeginDocument
                compile
                yield EventEndDocument

-- | Osm event compiler
compiler :: OsmEvent -> [Event]
compiler evnt = case evnt of
  EventGeoBounds g      -> boundE g
  EventMember    m      -> memberE m
  EventTag  k v         -> tagE k v
  EventNodeRef   nid    -> nodeRefE nid
  ----------------------------- Nested elements ----------------
  EventBeginOsm         -> [ begin "osm" osmAttr               ]
  EventEndOsm           -> [ end   "osm"                       ]
  EventNodeBegin n mt   -> [ begin "node"     $ nodeAttr n  mt ]
  EventNodeEnd          -> [ end   "node"                      ]
  EventWayBegin  mt     -> [ begin "way"      $ metaAttrs mt   ]
  EventWayEnd           -> [ end   "way"                       ]
  EventRelationBegin mt -> [ begin "relation" $ metaAttrs mt   ]
  EventRelationEnd      -> [ end   "relation"                  ]

----------------------------  Unnested elements ---------------


nodeRefE :: NodeID -> [Event]
nodeRefE nid = [begin "nd" [mkAttrS "ref" nid], end "nd"]

boundE :: GeoBounds -> [Event]
boundE = noBody "bounds" . attrsOfGB
  where attrsOfGB gb = [ mkAttrLens "minlat" minLatitude  gb
                       , mkAttrLens "maxlat" maxLatitude  gb
                       , mkAttrLens "minlon" minLongitude gb
                       , mkAttrLens "maxlon" maxLongitude gb
                       ]

memberE :: Member -> [Event]
memberE = noBody "member" . memAttr
  where memAttr (NodeM  rl oid)    = mAts "node" oid rl
        memAttr (WayM   rl oid)    = mAts "way"  oid rl
        memAttr (RelationM rl oid) = mAts "relation" oid rl
        mAts t o r = [ mkAttrS "ref"  o
                     , mkAttr "role" r
                     , mkAttr "type" t
                     ]

tagE :: Text -> Text -> [Event]
tagE k v = noBody "tag" [ mkAttr "k" k, mkAttr"v" v]


noBody :: Name -> [Attr] -> [Event]
noBody n ats = [begin n ats , end n]

-- | Begin an element.
begin :: Name -> [Attr] -> Event
begin = EventBeginElement


-- | End an element
end   :: Name -> Event
end   = EventEndElement

-------------------------------- Attributes -----------------------------------

type Attr = (Name, [Content])

-- | Make a single attribute.
mkAttr :: Name -> Text -> Attr
mkAttr n v = (n, [ContentText v])

mkAttrS :: Show a => Name -> a -> Attr
mkAttrS n = mkAttr n  . showT

mkAttrLens :: Show a => Name -> Lens' s a -> s -> Attr
mkAttrLens n lenz s = mkAttrS n $ s ^. lenz

-- | Begin for a node.
nodeAttr      :: Node -> OsmMeta Node -> [Attr]
nodeAttr n om = [ mkAttrLens "lat" latitude n
                , mkAttrLens "lon" longitude n
                ]
                ++ metaAttrs om

-- | Attrs of osm
osmAttr :: [Attr]
osmAttr = [ mkAttr"xmlns"     xmlNameSpace
          , mkAttr"version"  $ showVersionT osmXmlVersion
          , mkAttr"generator" naqshaVersionT
          ]


-- | Add osm meta information to the given generator.
metaAttrs :: OsmMeta e
          -> [Attr]
metaAttrs mt = catMaybes [ maybeAttr mt _osmID          $ mkAttrS "id"
                         , maybeAttr mt _modifiedUser   $ mkAttr "user"
                         , maybeAttr mt _modifiedUserID $ mkAttrS "uid"
                         , maybeAttr mt _timeStamp      $ mkAttr"timestamp" . showTime
                         , maybeAttr mt _version        $ mkAttrS "version"
                         , maybeAttr mt _changeSet      $ mkAttrS "changeset"
                         , maybeAttr mt _isVisible      $ visibleFunc
                         ]

  where maybeAttr :: a -> Lens' a (Maybe b) -> (b -> Attr) -> Maybe Attr
        maybeAttr a lns gen = gen <$> a ^. lns
        visibleFunc cond
          | cond          = mkAttr "visible" "true"
          | otherwise     = mkAttr "visible" "false"


---------------   Parsing osm events ------------------------------

-- | Conduit to convert Osm Events to xml.
parse :: Monad m => Parse m
parse = return ()


-- | Conduit to convert Osm Events to a complete xml document,
-- i.e. with preamble.
parseDoc :: Monad m => Parse m
parseDoc = do eb <- await
              case eb of
                Just EventBeginDocument -> parse
                _                       -> fail "expected xml preable"
