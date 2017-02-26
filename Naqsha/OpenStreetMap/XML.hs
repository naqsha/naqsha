{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
module Naqsha.OpenStreetMap.XML
       ( -- * Compiling to XML
         compile, compileDoc
       ) where

import  Control.Lens
import  Data.Conduit                ( Conduit, yield               )
import  Data.Conduit.List           ( concatMap                    )
import  Data.Maybe                  ( catMaybes                    )
import  Data.Text                   ( Text                         )
import  Data.XML.Types              ( Event(..), Name, Content(..) )
import  Prelude         hiding      ( concatMap                    )

import Naqsha.Common
import Naqsha.Position
import Naqsha.OpenStreetMap.Element
import Naqsha.OpenStreetMap.Stream

-- | OSM xml name space.
xmlNameSpace :: Text
xmlNameSpace = "http://openstreetmap.org/osm/0.6"

-- | Conduit to convert Osm Events to xml.
compile :: Monad m => Conduit OsmEvent m Event
compile = concatMap compiler

-- | Conduit to convert Osm Events to a complete xml document, i.e. with preamble.
compileDoc :: Monad m => Conduit OsmEvent m Event
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
nodeRefE nid = [begin "nd" [attrS "ref" nid], end "nd"]

boundE :: GeoBounds -> [Event]
boundE = noBody "bounds" . attrsOfGB
  where attrsOfGB gb = [ attrS "minlat" $ latitude  $ minGeo gb
                       , attrS "maxlat" $ latitude  $ maxGeo gb
                       , attrS "minlon" $ longitude $ minGeo gb
                       , attrS "maxlon" $ longitude $ maxGeo gb
                       ]

memberE :: Member -> [Event]
memberE = noBody "member" . memAttr
  where memAttr (NodeM  rl oid)    = mAts "node" oid rl
        memAttr (WayM   rl oid)    = mAts "way"  oid rl
        memAttr (RelationM rl oid) = mAts "relation" oid rl
        mAts t o r = [ attrS "ref"  o
                     , attr  "role" r
                     , attr  "type" t
                     ]

tagE :: Text -> Text -> [Event]
tagE k v = noBody "tag" [attr "k" k, attr "v" v]


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
attr :: Name -> Text -> Attr
attr n v = (n, [ContentText v])

attrS :: Show a => Name -> a -> Attr
attrS n = attr n . showT


-- | Begin for a node.
nodeAttr      :: Node -> OsmMeta Node -> [Attr]
nodeAttr n om = [ attrS "lat" $ latitude n
                , attrS "lon" $ longitude n
                ]
                ++ metaAttrs om

-- | Attrs of osm
osmAttr :: [Attr]
osmAttr = [ attr "xmlns"     xmlNameSpace
          , attr "version"  $ showVersionT osmXmlVersion
          , attr "generator" naqshaVersionT
          ]


-- | Add osm meta information to the given generator.
metaAttrs :: OsmMeta e
          -> [Attr]
metaAttrs mt = catMaybes [ maybeAttr mt _osmID          $ attrS "id"
                         , maybeAttr mt _modifiedUser   $ attr "user"
                         , maybeAttr mt _modifiedUserID $ attrS "uid"
                         , maybeAttr mt _timeStamp      $ attr "timestamp" . showTime
                         , maybeAttr mt _version        $ attrS "version"
                         , maybeAttr mt _changeSet      $ attrS "changeset"
                         , maybeAttr mt _isVisible      $ visibleFunc
                         ]

  where maybeAttr :: a -> Lens' a (Maybe b) -> (b -> Attr) -> Maybe Attr
        maybeAttr a lns gen = gen <$> a ^. lns
        visibleFunc cond
          | cond          = attr "visible" "true"
          | otherwise     = attr "visible" "false"
