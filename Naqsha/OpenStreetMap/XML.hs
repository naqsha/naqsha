{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Naqsha.OpenStreetMap.XML
       ( -- * Convertion between XML and OSM events.
         osmDoc, osm, translate
       , compile, compileDoc
       ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch         ( MonadThrow                      )
import           Control.Monad.State
import           Data.Conduit                ( Conduit, ConduitM, yield, await )
import           Data.Conduit.List           ( concatMap                       )
import           Data.Default
import           Data.Maybe                  ( catMaybes, fromMaybe            )
import           Data.Text                   ( Text, unpack                    )
import qualified Data.Vector     as V
import           Data.XML.Types              ( Event(..), Name, Content(..)    )
import           Prelude         hiding      ( concatMap                       )
import           Text.XML.Stream.Parse

import Naqsha.Common
import Naqsha.Position
import Naqsha.OpenStreetMap.Element
import Naqsha.OpenStreetMap.Stream

-- | Conduit that converts OsmEvents to xml events.
type Compile   m = Conduit  OsmEvent m Event

-- | Conduit that converts XML events to the corresponding OsmEvents.
type Trans     m = Conduit  Event    m OsmEvent

-- | Translate a single xml element to the corresponding set of
-- OsmEvents.
type ElemTrans m = ConduitM Event    OsmEvent m (Maybe ())

-- | OSM xml name space.
xmlNameSpace :: Text
xmlNameSpace = "http://openstreetmap.org/osm/0.6"


-- | Translate the top level osm element
osm  :: MonadThrow m => ElemTrans m
osm  = tagName "osm" ignoreAttrs (const osmBody)
  where osmBody = betweenC EventBeginOsm EventEndOsm translate

-- | Translate bounds, nodes, ways, and relations.
translate :: MonadThrow m => Trans m
translate = transBody [boundsT, nodeT, wayT, relationT]

-- | Conduit to convert an XML document into the corresponding OSM
-- events.
osmDoc :: MonadThrow m => Trans m
osmDoc = do void $ await >>= check errBegin
            osm
            void $ await >>= check errEnd
  where errBegin  = fail "xml preamble missing"
        errEnd    = fail "xml document is not completely parsed"
        check err = fromMaybe err . fmap return


-- | Conduit to convert Osm Events to xml.
compile :: Monad m => Compile m
compile = concatMap compiler

-- | Conduit to convert Osm Events to a complete xml document,
-- i.e. with preamble.
compileDoc :: Monad m => Compile m
compileDoc = betweenC EventBeginDocument EventEndDocument compile


-- | Osm event compiler
compiler :: OsmEvent -> [Event]
compiler evnt = case evnt of
  EventGeoBounds g      -> boundE g
  EventMember    m      -> memberE m
  EventTag  k v         -> osmTagE k v
  EventNodeRef   nid    -> nodeRefE nid
  ----------------------------- Nested elements ----------------
  EventBeginOsm         -> [ EventBeginElement "osm" osmAttr               ]
  EventEndOsm           -> [ EventEndElement   "osm"                       ]
  EventNodeBegin n mt   -> [ EventBeginElement "node"     $ nodeAttr n  mt ]
  EventNodeEnd          -> [ EventEndElement   "node"                      ]
  EventWayBegin  mt     -> [ EventBeginElement "way"      $ metaAttrs mt   ]
  EventWayEnd           -> [ EventEndElement   "way"                       ]
  EventRelationBegin mt -> [ EventBeginElement "relation" $ metaAttrs mt   ]
  EventRelationEnd      -> [ EventEndElement   "relation"                  ]

----------------------------  Unnested elements ---------------


nodeRefE :: NodeID -> [Event]
nodeRefE nid = [EventBeginElement "nd" [mkAttrS "ref" nid], EventEndElement "nd"]

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


osmTagE :: Text -> Text -> [Event]
osmTagE k v = noBody "tag" [ mkAttr "k" k, mkAttr"v" v]

-- | Element with empty body.
noBody :: Name -> [Attr] -> [Event]
noBody n ats = [EventBeginElement n ats , EventEndElement n]

-------------------------------- Attributes makers-----------------------------------

type Attr = (Name, [Content])

-- | Make a single attribute.
mkAttr :: Name -> Text -> Attr
mkAttr n v = (n, [ContentText v])

-- | Make an attribute using the show instance of the value.
mkAttrS :: Show a => Name -> a -> Attr
mkAttrS n = mkAttr n  . showT

-- | Make an attribute using a getter for the attribute value.
mkAttrLens :: Show a => Name -> Getter s a -> s -> Attr
mkAttrLens n lenz s = mkAttrS n $ s ^. lenz

-- | Attributes for a node element.
nodeAttr      :: Node -> OsmMeta Node -> [Attr]
nodeAttr n om = [ mkAttrLens "lat" latitude n
                , mkAttrLens "lon" longitude n
                ]
                ++ metaAttrs om

-- | Attributes for an osm element.
osmAttr :: [Attr]
osmAttr = [ mkAttr "xmlns"     xmlNameSpace
          , mkAttr "version" $ showVersionT osmXmlVersion
          , mkAttr "generator" naqshaVersionT
          ]


-- | Attributes associated with meta information to the given generator.
metaAttrs :: OsmMeta e
          -> [Attr]
metaAttrs mt = catMaybes [ maybeAttr mt _osmID          $ mkAttrS "id"
                         , maybeAttr mt _modifiedUser   $ mkAttr "user"
                         , maybeAttr mt _modifiedUserID $ mkAttrS "uid"
                         , maybeAttr mt _timeStamp      $ mkAttr "timestamp" . showTime
                         , maybeAttr mt _version        $ mkAttrS "version"
                         , maybeAttr mt _changeSet      $ mkAttrS "changeset"
                         , maybeAttr mt _isVisible      $ visibleFunc
                         ]

  where maybeAttr :: a -> Lens' a (Maybe b) -> (b -> Attr) -> Maybe Attr
        maybeAttr a lns gen = gen <$> a ^. lns
        visibleFunc cond
          | cond          = mkAttr "visible" "true"
          | otherwise     = mkAttr "visible" "false"


---------------   Translating XML events to Osm Events ------------------------------------------

-- | Translate a bounds element.
boundsT :: MonadThrow m => ElemTrans m
boundsT = tagName "bounds" bAttr $ yield . EventGeoBounds
  where bAttr = toAttrParser def $ do
          toAttrSetParser maxLatitude  $ angularAttrP "maxlat"
          toAttrSetParser maxLongitude $ angularAttrP "maxlon"
          toAttrSetParser minLatitude  $ angularAttrP "minlat"
          toAttrSetParser minLongitude $ angularAttrP "minlon"

-- | Translate a node element.
nodeT :: MonadThrow m => ElemTrans m
nodeT = tagName "node" nAttr nBody
  where nBody (g,mt) = betweenC (EventNodeBegin g mt) EventNodeEnd $ transElements osmTagT
        geoAttr = toAttrParser def $ do
          toAttrSetParser latitude  $ angularAttrP "lat"
          toAttrSetParser longitude $ angularAttrP "lon"
        nAttr = (,) <$> geoAttr <*> metaAttrP

-- | Translate a way element
wayT :: MonadThrow m => ElemTrans m
wayT = tagName "way" metaAttrP wBody
  where wBody mt = betweenC (EventWayBegin mt) EventWayEnd $ transBody [nodeRefT, osmTagT]

-- | Translate a relation element.
relationT :: MonadThrow m => ElemTrans m
relationT = tagName "relation" metaAttrP rBody
  where rBody mt = betweenC (EventRelationBegin mt) EventRelationEnd $ transBody [memberT, osmTagT]



-- | Translate an osm elemnt.
osmTagT :: MonadThrow m => ElemTrans m
osmTagT = tagName "tag" kvAttr yield
  where kvAttr = EventTag <$> requireAttr "k" <*> requireAttr "v"

-- | Translate a member element
memberT :: MonadThrow m => ElemTrans m
memberT = tagName "member" mAttr $ yield . EventMember
  where mAttr = do r <- requireAttr "role"
                   t <- requireAttr "type"
                   case t of
                     "node"     -> NodeM     r <$> refAttrP
                     "way"      -> WayM      r <$> refAttrP
                     "relation" -> RelationM r <$> refAttrP
                     _          -> fail "bad member type"


-- | Translate a node reference.
nodeRefT :: MonadThrow m => ElemTrans m
nodeRefT = tagName "nd" refAttrP $ yield . EventNodeRef


----------------------------- Some Helper Conduit -------------------------


-- | Translate the body of the element.
transBody :: MonadThrow m
          => [ElemTrans m]  -- ^ translators for each element of the body
          -> Trans m
transBody = transElements . choose

-- | Translate a stream of
transElements :: Monad m
              => ElemTrans m  -- ^ The translator for a single element.
              -> Trans m
transElements pr = pr >>= checkAndRun
   where checkAndRun = maybe (return ()) $ const $ transElements pr


-- | Emit a preamble and a epilogue for the stream.
betweenC :: Monad m
         => o             -- ^ The preamble
         -> o             -- ^ The epilogue
         -> Conduit i m o
         -> Conduit i m o
betweenC b e pr = yield b >> pr >> yield e




-------------------------  Attribute parsers ----------------------------------


-- | Attribute parser to parse an angular quantity like latitude,
-- longitude etc.
angularAttrP :: (Angular a, Read a)
             => Name
             -> AttrParser a
angularAttrP nm = force err $ readMaybeT <$> requireAttr nm
  where err = "bad " ++ show nm


refAttrP  :: AttrParser (OsmID a)
refAttrP  =  force err $ readOsmID <$> requireAttr "ref"
  where err = "bad osm id"

metaAttrP :: AttrParser (OsmMeta a)
metaAttrP = toAttrParser def $ do
  toAttrSetParser _osmID          $ attrConvP "id"        (fmap OsmID . readMaybeT)
  toAttrSetParser _modifiedUser   $ attr      "user"
  toAttrSetParser _modifiedUserID $ attrConvP "uid"       readMaybeT
  toAttrSetParser _timeStamp      $ attrConvP "timestamp" timeParser
  toAttrSetParser _version        $ attrConvP "version"   readMaybeT
  toAttrSetParser _changeSet      $ attrConvP "changeset" readMaybeT
  toAttrSetParser _isVisible      $ attrConvP "visible"   visibleConv
  where attrConvP :: Name                 -- ^ name of the attribute
                  -> (Text -> Maybe x)    -- ^ text to x converter
                  -> AttrParser (Maybe x)
        attrConvP name conv = (>>= conv) <$> attr name
        visibleConv "true"  = Just True
        visibleConv "false" = Just False
        visibleConv _       = Nothing

-- | An attribute parser that sets the attribute values.
type AttrSetParser s = StateT s AttrParser ()

-- | Convert an attribute setting parser to a normal attribute parser.
toAttrParser :: s -> AttrSetParser s -> AttrParser s
toAttrParser = flip execStateT

-- | Create an attribute setting parser out of a given setter.
toAttrSetParser :: Setter' s a  -- ^ The setter to use
                -> AttrParser a -- ^ parser for the value to be set.
                -> AttrSetParser s
toAttrSetParser setter ap = do x <- lift ap;
                               setter .= x
