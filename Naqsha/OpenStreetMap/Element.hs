{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE Rank2Types                 #-}

-- | The basic elements of open street map.
module Naqsha.OpenStreetMap.Element
       (

       -- * Open Street Map elements.
         Node, Way, Relation, Member(..)
       -- ** Sematic elements.
       , Tagged, OsmTags, OsmTagged(..)
       , Osm, OsmElement(..), unMeta
       , OsmMeta, OsmID(..), unsafeToOsmID, readOsmID
       , NodeID, WayID, RelationID
       -- ** Useful Lenses.
       , tagAt, wayNodes, relationMembers
       , osmID, modifiedUser, modifiedUserID, timeStamp, changeSet, version
       , isVisible
       -- *** Lenses for OsmMeta
       , _osmID, _modifiedUser, _modifiedUserID, _timeStamp, _changeSet, _version
       , _isVisible
       ) where

import           Control.Monad                  ( liftM )
import           Control.Lens
import           Data.Default
import qualified Data.HashMap.Lazy           as HM
import           Data.Text   hiding             (empty)
import           Data.Time
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GVM
import           Data.Vector.Unboxed            (Vector, MVector, Unbox)

import           Data.Word

import Naqsha.Position
import Naqsha.Common



-- $osm$
--
-- The Open street map describes the world using three kinds of
-- elements given by the types `Node`, `Way` and `Relation`
-- respectively. Intuitively, an element of type `Node` captures a
-- location, a `Way` captures a path and a `Relation` captures a
-- combination of other elements, each in a specific _role_.
--
-- = Sematics of Elements.
--
-- While the basic nature of an osm element is given by its type,
-- there are additional semantics that needs to be given to make it
-- possible to represent an element on the map. For example, a node
-- could be just an intermediate point in a path or might have a more
-- significant semantic like being a bus stop on a bus route. A way
-- might be a road, a boundary for a region or a river. Open streetmap
-- associates such semantics to an element through a set of tags
-- captured by the type `OsmTags`. The type @`Tagged` e@ captures
-- elements of type @e@ glued with a set of tags which fully describe
-- the semantics of the object.
--
-- = Database meta information
--
-- The Open street map infrastructure also keeps track of some meta
-- information that helps managing the elements in the database.  One
-- of the most important information that is kept track of is the
-- element id, captured by the type `OsmID`. The element id servers as
-- a unique reference to objects in the data base and is also used
-- inside elements like `Way` and `Relation`. The meta information
-- also has other data like object revision number, change set,
-- etc. which are important if one wants to edit the Open Street Map
-- database.

------------------------ Element Identifiers ---------------------

-- | The ID of an object in open street map. Currently, the Open
-- Street map project uses 64-bit word for ids. We use the phantom
-- type of the entity for better type safety.
newtype OsmID element  = OsmID Word64 deriving (Eq, Ord)

-- | Convert the word64 into an OsmID. Exposed only for internal
-- modules not to be exported outside.
unsafeToOsmID :: Word64 -> OsmID e
unsafeToOsmID = OsmID

-- | Read the OSM id from text.
readOsmID :: Text -> Maybe (OsmID a)
readOsmID = fmap OsmID . readMaybeT


instance Show (OsmID element) where
  show (OsmID x) = show x

instance Unbox (OsmID element)

newtype instance MVector s (OsmID element) = MOsmIDV  (MVector s Word64)
newtype instance Vector    (OsmID element) = OsmIDV   (Vector Word64)

instance GVM.MVector MVector (OsmID element) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength          (MOsmIDV v)          = GVM.basicLength v
  basicUnsafeSlice i n (MOsmIDV v)          = MOsmIDV $ GVM.basicUnsafeSlice i n v
  basicOverlaps (MOsmIDV v1) (MOsmIDV v2)     = GVM.basicOverlaps v1 v2

  basicUnsafeRead  (MOsmIDV v) i            = OsmID `liftM` GVM.basicUnsafeRead v i
  basicUnsafeWrite (MOsmIDV v) i (OsmID x)  = GVM.basicUnsafeWrite v i x

  basicClear (MOsmIDV v)                    = GVM.basicClear v
  basicSet   (MOsmIDV v)         (OsmID x)  = GVM.basicSet v x

  basicUnsafeNew n                        = MOsmIDV `liftM` GVM.basicUnsafeNew n
  basicUnsafeReplicate n     (OsmID x)    = MOsmIDV `liftM` GVM.basicUnsafeReplicate n x
  basicUnsafeCopy (MOsmIDV v1) (MOsmIDV v2)   = GVM.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MOsmIDV v)   n           = MOsmIDV `liftM` GVM.basicUnsafeGrow v n

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MOsmIDV v)               = GVM.basicInitialize v
#endif

instance GV.Vector Vector (OsmID element) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MOsmIDV v)         = OsmIDV  `liftM` GV.basicUnsafeFreeze v
  basicUnsafeThaw (OsmIDV v)            = MOsmIDV `liftM` GV.basicUnsafeThaw v
  basicLength (OsmIDV v)                = GV.basicLength v
  basicUnsafeSlice i n (OsmIDV v)       = OsmIDV $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM (OsmIDV v) i        = OsmID   `liftM`  GV.basicUnsafeIndexM v i

  basicUnsafeCopy (MOsmIDV mv) (OsmIDV v) = GV.basicUnsafeCopy mv v
  elemseq _ (OsmID x)                 = GV.elemseq (undefined :: Vector a) x


------------------------- Semantic Tags ---------------------------------

-- | The tags of an OSM element.
type OsmTags = HM.HashMap Text Text

-- | A tagged element.
data Tagged e = Tagged { __element :: e
                       , __tags    :: OsmTags
                       }

makeLenses ''Tagged

-- | The default value is with empty tags.
instance Default e => Default (Tagged e) where
  def = Tagged def HM.empty

instance Location e => Location (Tagged e) where
  latitude    = _element . latitude
  longitude   = _element . longitude
  geoPosition = _element . geoPosition


-- | Family of types that have Open street map tags.
class OsmTagged a where

  -- | The type family that captures the underlying element.
  type ElementType a :: *

  -- | Lens to focus on the tags
  tags     :: Lens' a OsmTags

  -- | Lens to focus on the untaged element.
  untagged :: Lens' a (ElementType a)

-- | The untagged element type is e
instance OsmTagged (Tagged e) where
  type ElementType (Tagged e) = e
  tags     = _tags
  untagged = _element

---------------- Open street map meta data --------------------

-- | An fully qualified osm element. The type @Osm e@ should be seen
-- as elements of type @e@ glued with a set of osm tags together with
-- the meta data for the object in the Open street map database.
data Osm e = Osm { __osmTaggedElement :: Tagged e
                 , __osmMeta          :: OsmMeta e
                 }

-- | The open street map metadata that is associated with each
-- element.
data OsmMeta a = OsmMeta { __osmID          :: Maybe (OsmID a)
                         , __modifiedUser   :: Maybe Text
                         , __modifiedUserID :: Maybe Integer
                         , __isVisible      :: Maybe Bool
                         , __version        :: Maybe Integer
                         , __timeStamp      :: Maybe UTCTime
                         , __changeSet      :: Maybe Integer
                         }

makeLenses ''OsmMeta
makeLenses ''Osm

instance Default (OsmMeta a) where
  def = OsmMeta { __osmID          = Nothing
                , __modifiedUser   = Nothing
                , __modifiedUserID = Nothing
                , __isVisible      = Nothing
                , __version        = Nothing
                , __timeStamp      = Nothing
                , __changeSet      = Nothing
                }


instance Default e => Default (Osm e) where
  def = Osm def def

-- | The associated untagged element is e.
instance OsmTagged (Osm e) where
  type ElementType (Osm e) = e
  tags     = _osmTaggedElement . _tags
  untagged = _osmTaggedElement . _element

instance Location e => Location (Osm e) where
  latitude    = untagged . latitude
  longitude   = untagged . longitude
  geoPosition = untagged . geoPosition

-- | Class that captures elements that behave like a fully for some e
class OsmTagged a => OsmElement a where
  meta :: Lens' a (OsmMeta (ElementType a))

instance OsmElement (Osm e) where
  meta = _osmMeta


-- | Only peal of the meta data but get the tagged variant.
unMeta :: OsmElement a => a -> Tagged  (ElementType a)
unMeta a = Tagged { __element = a ^. untagged
                  , __tags    = a ^. tags
                  }

-------------- Some useful lenses -------------------------------

-- | Lens to focus on the tag at a given key.
tagAt :: OsmTagged a => Text -> Lens' a (Maybe Text)
tagAt k = tags . at k

-- | Lens to focus on the Id of the element.
osmID :: OsmElement a => Lens' a (Maybe (OsmID (ElementType a)))
osmID = meta . _osmID

-- | Lens to focus on the user who last modified.
modifiedUser  :: OsmElement a => Lens' a (Maybe Text)
{-# INLINE modifiedUser #-}
modifiedUser = meta . _modifiedUser

-- | Lens to focus on the user id of the user that last modified.
modifiedUserID :: OsmElement a => Lens' a (Maybe Integer)
modifiedUserID = meta . _modifiedUserID

-- | Flag which indicates whether the associated element is visible or
-- not.
isVisible :: OsmElement a => Lens' a (Maybe Bool)
isVisible  = meta . _isVisible


-- | The version number of the associated entry.
version :: OsmElement a => Lens' a (Maybe Integer)
version =  meta . _version

-- | The time stamp (utc) when the entry was last changed.
timeStamp ::OsmElement a =>  Lens' a (Maybe UTCTime)
timeStamp = meta . _timeStamp

-- | The change set number where the object was changed.
changeSet :: OsmElement a => Lens' a (Maybe Integer)
changeSet = meta . _changeSet

---------- The element of Open street map -----------------

type    Node       = Geo

-- | ID of Nodes
type    NodeID     = OsmID Node

-- | ID of Ways
type    WayID      = OsmID Way

-- | ID of Relations
type    RelationID = OsmID Relation


-- | The primitive way type.
newtype Way  = Way       { __wayNodes        :: Vector NodeID }

-- | A member of a relation.

data Member = NodeM     Text NodeID
            | WayM      Text WayID
            | RelationM Text RelationID

-- | The primitive relation type.
newtype Relation = Relation { __relationMembers :: V.Vector Member }

makeLenses ''Way
makeLenses ''Relation

-- | Lens to focus on the node ids in the way.
wayNodes :: Lens' Way (Vector NodeID)
wayNodes =  _wayNodes

-- | Lens to focus on the members of a relation.
relationMembers  :: Lens' Relation (V.Vector Member)
relationMembers = _relationMembers
