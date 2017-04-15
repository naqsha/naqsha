{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE ExistentialQuantification #-}
module Naqsha.OpenStreetMap.XML
       ( -- * Processing Open Street Map XML.
         -- $xmlproc$
         eventsFromFile
       , parse, asXML
       , asPrettyXML
       -- ** Conduits to translate to and from XML.
       , compile, compileDoc
       , osm
       ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch         ( MonadThrow                      )
import           Control.Monad.Primitive     ( PrimMonad                       )
import           Control.Monad.Base          ( MonadBase                       )
import           Control.Monad.State
import           Control.Monad.Trans.Resource( MonadResource                   )
import           Data.ByteString             ( ByteString                      )
import           Data.Conduit                ( Conduit, ConduitM, yield, Source
                                             , (=$=), Producer
                                             )
import           Data.Conduit.List           ( concatMap                       )
import           Data.Conduit.Combinators    ( peek                            )
import           Data.Default
import           Data.Maybe                  ( catMaybes                       )
import           Data.Text                   ( Text                            )
import           Data.XML.Types              ( Event(..), Name, Content(..)    )
import           Prelude         hiding      ( concatMap                       )
import           Text.XML.Stream.Render      ( renderBytes, rsPretty           )
import           Text.XML.Stream.Parse

import Naqsha.Common
import Naqsha.Position
import Naqsha.OpenStreetMap.Element
import Naqsha.OpenStreetMap.Stream

-- $xmlproc$
--
-- This module provides a streaming interface to process osm's xml
-- files. The basic combinators are `eventsFromFile`, which streams an
-- xml file as a stream of OSMEvents, and `asXML` which converts an
-- stream of OSM events into the corresponding xml file.

-- | Name associated with osm
osmName :: Name
osmName = "{http://openstreetmap.org/osm/0.6}osm"

---------------------------  The translators and compilers ------------------------------------

-- | Conduit to convert Osm Events to xml.
compile :: Monad m => Conduit OsmEvent m Event
compile = concatMap compiler

-- | Conduit to convert Osm Events to a complete xml document,
-- i.e. with preamble.
compileDoc :: Monad m => Conduit OsmEvent m Event
compileDoc = betweenC EventBeginDocument EventEndDocument compile


-- | Osm event compiler
compiler :: OsmEvent -> [Event]
compiler evnt = case evnt of
  EventGeoBounds g      -> boundE g
  EventMember    m      -> memberE m
  EventTag  k v         -> osmTagE k v
  EventNodeRef   nid    -> nodeRefE nid
  ----------------------------- Nested elements ----------------
  EventBeginOsm         -> [ EventBeginElement osmName osmAttr             ]
  EventEndOsm           -> [ EventEndElement   osmName                     ]
  EventNodeBegin mt n   -> [ EventBeginElement "node"     $ nodeAttr n  mt ]
  EventNodeEnd          -> [ EventEndElement   "node"                      ]
  EventWayBegin  mt     -> [ EventBeginElement "way"      $ metaAttrs mt   ]
  EventWayEnd           -> [ EventEndElement   "way"                       ]
  EventRelationBegin mt -> [ EventBeginElement "relation" $ metaAttrs mt   ]
  EventRelationEnd      -> [ EventEndElement   "relation"                  ]



-- | Convert osm events to an xml file. It is the responsibility of
-- the input conduit to ensure that it gives a well formed set of Osm
-- events.
asXML :: (PrimMonad base, MonadBase base m)
      => OsmSource m             -- ^ The event source to render as xml
      -> Source m ByteString
asXML src = src =$= compileDoc =$= renderBytes def

-- | Convert osm events to a pretty printed xml file. It is the
-- responsibility of the input conduit to ensure that it gives a well
-- formed set of Osm events.
asPrettyXML :: (PrimMonad base, MonadBase base m)
            => OsmSource m
            -> Source m ByteString
asPrettyXML src = src =$= compileDoc =$= renderBytes settings
  where settings = def { rsPretty     = True }


-- | Translate a byte stream corresponding to an osm xml file into a
-- stream of `OsmEvent`s.
parse :: MonadThrow m =>  Conduit ByteString m OsmEvent
parse = parseBytes def =$= osm


-- | Stream the osm events from an xml file.
eventsFromFile :: MonadResource m => FilePath -> Producer m OsmEvent
eventsFromFile fp = parseFile def fp =$= osm


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
osmAttr = [ mkAttr "version" $ showVersionT osmXmlVersion
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


-- | Conduit that converts XML events to the corresponding OsmEvents.
type Trans     m = Conduit  Event    m OsmEvent

-- | Translate that signals failure with a Maybe.
type TryTrans m  = ConduitM Event OsmEvent m (Maybe ())

type TagParser m  = (Name, Match m)


data Match  m  = forall a . Match (AttrParser a) (a -> Trans m)


-- | Try running the match.
tryTag :: MonadThrow m => TagParser m -> TryTrans m
tryTag (nm, (Match atp run)) = tagName nm atp run


body  :: MonadThrow m => Name -> [TagParser m] -> Trans m
body nm choices = go [] choices
  where go tried prs = case prs of
          tp@(e,_) : prs' -> tryTag tp >>= continue (e:tried) prs'
          []              -> peek      >>= maybe (err "eof encountered") closing


        continue tried ps = maybe (go tried ps) $ const $ body nm choices
        closing (EventEndElement nmp)
          | nmp == nm = return ()
          | otherwise = err "bad nesting"
        closing _ = return ()

        err  msg   = fail $ "osm-xml: <"  ++ show nm ++ "> " ++ msg

matchTag :: MonadThrow m => TagParser m -> Trans m
matchTag tp@(nm,_) = tryTag tp >>= maybe err return
  where err = fail $ "osm-xml: unable to match <" ++ show nm ++ ">"



------------------- Actual parsers -----------------------------------------------------------------

-- | Tag matcher for tags that do not have a body.
tagNoBodyP :: Monad m
           => Name            -- ^ name of the tag
           -> AttrParser a    -- ^ the attribute parser
           -> (a -> OsmEvent) -- ^ function to generate the event.
           -> TagParser m
tagNoBodyP nm atp func = (nm, Match atp (yield . func))


-- | Construct a matcher for a general element.
tagP :: MonadThrow m
     => Name            -- ^ name of the tag
     -> AttrParser a    -- ^ attribute parser
     -> (a -> OsmEvent) -- ^ the start of the tag
     -> OsmEvent        -- ^ the end of the tag
     -> [TagParser m]   -- ^ The body of the tag
     -> TagParser m
tagP nm atp str ed bd = (nm, Match atp continue)
  where continue a = betweenC (str a) ed $ body nm bd



-- | Translate the top level osm element
osm  :: MonadThrow m => Conduit Event m OsmEvent
osm  = matchTag $ tagP osmName ignoreAttrs (const EventBeginOsm) EventEndOsm [boundsP, nodeP, wayP, relationP]


----------------------- Matchers for different elements ----------------------------------

-- | Translate a bounds element.
boundsP :: Monad m => TagParser m
boundsP = tagNoBodyP "bounds" bAttr EventGeoBounds
  where bAttr = buildM $ do
          toAttrSetParser maxLatitude  $ angularAttrP "maxlat"
          toAttrSetParser maxLongitude $ angularAttrP "maxlon"
          toAttrSetParser minLatitude  $ angularAttrP "minlat"
          toAttrSetParser minLongitude $ angularAttrP "minlon"

nodeP :: MonadThrow m => TagParser m
nodeP = tagP "node" nAttr (uncurry EventNodeBegin) EventNodeEnd [osmTagP]
  where geoAttr =  buildM $ do
          toAttrSetParser latitude  $ angularAttrP "lat"
          toAttrSetParser longitude $ angularAttrP "lon"
        nAttr = (,) <$> metaAttrP <*> geoAttr

-- | Translate a way element
wayP :: MonadThrow m => TagParser m
wayP = tagP "way" metaAttrP EventWayBegin EventWayEnd [nodeRefP, osmTagP]

-- | Translate a relation element.
relationP :: MonadThrow m => TagParser m
relationP = tagP "relation" metaAttrP EventRelationBegin EventRelationEnd [memberP, osmTagP]

-- | Translate an osm elemnt.
osmTagP :: Monad m => TagParser m
osmTagP = tagNoBodyP "tag" kvAttr id
  where kvAttr = EventTag <$> requireAttr "k" <*> requireAttr "v"

-- | Translate a member element
memberP :: Monad m => TagParser m
memberP = tagNoBodyP "member" mAttr $ EventMember
  where mAttr = do r <- requireAttr "role"
                   t <- requireAttr "type"
                   case t of
                     "node"     -> NodeM     r <$> refAttrP
                     "way"      -> WayM      r <$> refAttrP
                     "relation" -> RelationM r <$> refAttrP
                     _          -> fail "bad member type"


-- | Translate a node reference.
nodeRefP :: Monad m => TagParser m
nodeRefP = tagNoBodyP "nd" refAttrP $ EventNodeRef


----------------------------- Some Helper Conduit -------------------------

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
metaAttrP = buildM $ do
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

-- | Create an attribute setting parser out of a given setter.
toAttrSetParser :: Setter' s a  -- ^ The setter to use
                -> AttrParser a -- ^ parser for the value to be set.
                -> AttrSetParser s
toAttrSetParser setter atp = do x <- lift atp;
                                setter .= x
