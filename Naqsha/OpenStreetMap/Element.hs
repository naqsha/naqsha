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
       -- $osm$
         Node, Way, Relation
       , wayNodes, relationMembers
       , Member(..)
       -- ** Building/Updating via lens interface.
       , build, withChanges, buildM, withChangesM
       -- * Semantic Tags.
       , Tagged, OsmTags
       , OsmTagged(..)
       , tagAt
       -- * Database and its types.
       -- $database$
       --
       , OsmID(..)
       , NodeID, WayID, RelationID
       , unsafeToOsmID, readOsmID
       -- ** Meta data.
       , Osm
       -- *** Lenses to access the meta data.
       , osmID, modifiedUser, modifiedUserID, timeStamp, changeSet, version
       , isVisible, unMeta, meta
       -- ** The metadata type.
       , OsmMeta
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
import qualified Data.Vector.Unboxed         as VU
import           Data.Word

import Naqsha.Position
import Naqsha.Common


-- $osm$
--
-- The Open street map describes the world using three kinds of
-- elements given by the types `Node`, `Way` and `Relation`
-- respectively. Intuitively, an element of type `Node` captures a
-- location, a `Way` captures path traversed on the globe, and a
-- relation is a collection of nodes and ways which together forms
-- some interesting cartographic entity.
--
-- == Semantics of Elements and Lenses
--
-- The three elements described above have some basic semantic
-- information. For example, a `Node` has a `latitude` and a
-- `longitude`, a way is an ordered list of nodes that forms the
-- continuous curve on the globe, and a relation is on a collection of
-- its members nodes and ways. These basic information can be accessed
-- via the lenses `latitude`, `longitude`, `wayNodes`, and
-- `relationMembers`.
--
-- Besides these basic semantic content, elements often have other
-- cartographic information associated with them.  For example, a node
-- might be a location of a bus stop. A `Way` might be a road or a
-- boundary of a region or a river. Open Street Map associates all
-- such through a collection of optional @(key,value)@ pairs called
-- tags and naqsha provides lenses to access these tags. See the
-- module "Naqsha.OpenStreetMap.Tags" for some standard tags.
--
-- === Examples.
--
-- We also expose a lens based creation and updation interface using
-- the `build` and `withChanges` combinators.
--
-- > import Control.Lenses
-- > import Naqsha.OpenStreetMap
-- >
-- > kanpur = Osm Node
-- > kanpur = build $ do latitude  .= lat 26.4477777  -- basic information
-- >                     longitude .= lon 80.3461111  -- basic information
-- >                     name      .= Just "Kanpur"   -- the name tag. Notice the Just
-- >
-- >
-- > kanpurHindi = kanpur `withChanges` do
-- >       nameIn hindi .= Just "कानपुर" -- adds a multi-lingual name.
-- >
-- > -- Unsets the elevation tag of  node
-- > unsetElevation :: Osm Node -> Osm Node
-- > unsetElevation x = x `withChanges` do elevation .= Nothing
--
-- Notice that all lenses corresponding to tags focus on a value of
-- type @`Maybe` v@. This is because, all tags in Open Street Map are
-- optional and can be set/unset using `Just` and `Nothing`
-- respectively.


-- $database$
--
-- Elements in Open Street Map are stored in a database and are thus
-- associated with some metadata. For example, elements have database
-- ID's captured by the data type `OsmID`.  This serves as a unique
-- reference for the element in the data base.  The meta information
-- also has other data like object revision number, change set,
-- etc. The type `OsmMeta` captures these meta information. An
-- element, together with its database metadata is captured by the
-- type `Osm`.

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
                       } deriving (Show, Eq)

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
data Osm e = Osm { __osmTaggedElement  :: Tagged e
                 , __osmMeta           :: OsmMeta e
                 } deriving (Show, Eq)

-- | The open street map metadata that is associated with each
-- element.
data OsmMeta a = OsmMeta { __osmID          :: Maybe (OsmID a)
                         , __modifiedUser   :: Maybe Text
                         , __modifiedUserID :: Maybe Integer
                         , __isVisible      :: Maybe Bool
                         , __version        :: Maybe Integer
                         , __timeStamp      :: Maybe UTCTime
                         , __changeSet      :: Maybe Integer
                         } deriving (Show, Eq)

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

-- | Strip off the meta data and return the underlying tagged data.
unMeta :: Lens' (Osm e) (Tagged e)
unMeta = _osmTaggedElement

-- | Lens to focus on the meta information of the element.
meta :: Lens' (Osm e) (OsmMeta e)
meta = _osmMeta

-------------- Some useful lenses -------------------------------

-- | Lens to focus on the tag at a given key. A more type safe method
-- to access tags is given in the module
-- "Naqsha.OpenStreetMap.Tags". Always prefer the one exposed from
-- this module instead of using `tagAt` directly.
tagAt :: OsmTagged a => Text -> Lens' a (Maybe Text)
tagAt k = tags . at k

-- | Lens to focus on the Id of the element.
osmID :: Lens' (Osm a) (Maybe (OsmID  a))
osmID = _osmMeta . _osmID

-- | Lens to focus on the user who last modified.
modifiedUser  :: Lens' (Osm a) (Maybe Text)
{-# INLINE modifiedUser #-}
modifiedUser = _osmMeta . _modifiedUser

-- | Lens to focus on the user id of the user that last modified.
modifiedUserID :: Lens' (Osm a) (Maybe Integer)
modifiedUserID = _osmMeta . _modifiedUserID

-- | Flag which indicates whether the associated element is visible or
-- not.
isVisible :: Lens' (Osm a) (Maybe Bool)
isVisible  = _osmMeta . _isVisible

-- | The version number of the associated entry.
version :: Lens' (Osm a) (Maybe Integer)
version =  _osmMeta . _version

-- | The time stamp (utc) when the entry was last changed.
timeStamp :: Lens' (Osm a) (Maybe UTCTime)
timeStamp = _osmMeta . _timeStamp

-- | The change set number where the object was changed.
changeSet :: Lens' (Osm a) (Maybe Integer)
changeSet = _osmMeta . _changeSet

---------- The element of Open street map -----------------

type    Node       = Geo

-- | ID of Nodes
type    NodeID     = OsmID Node

-- | ID of Ways
type    WayID      = OsmID Way

-- | ID of Relations
type    RelationID = OsmID Relation


-- | The primitive way type.
newtype Way  = Way       { __wayNodes        :: Vector NodeID } deriving (Show, Eq)

instance Default Way where
  def = Way VU.empty
-- | A member of a relation.

data Member = NodeM     Text NodeID
            | WayM      Text WayID
            | RelationM Text RelationID deriving (Show, Eq)

-- | The primitive relation type.
newtype Relation = Relation { __relationMembers :: V.Vector Member } deriving (Show, Eq)

instance Default Relation where
  def = Relation V.empty

makeLenses ''Way
makeLenses ''Relation

-- | Lens to focus on the node ids in the way.
wayNodes :: Lens' Way (Vector NodeID)
wayNodes =  _wayNodes

-- | Lens to focus on the members of a relation.
relationMembers  :: Lens' Relation (V.Vector Member)
relationMembers = _relationMembers
