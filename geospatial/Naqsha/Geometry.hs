-- | The geometric types and values exposed by naqsha.
module Naqsha.Geometry
       (
       -- * Angles
         module Naqsha.Geometry.Angle.Internal
       -- * Latitude, Longitude and LatLng points
       -- $latandlong$
       , module Naqsha.Geometry.LatLon.Internal
       -- * Geometric hashing.
         -- $geohashing$
       , module Naqsha.Geometry.GeoHash

       -- * Distance calculation.
       -- $distance$
       
       
       -- * Internal details
       -- $internals$

       ) where


import Naqsha.Geometry.Angle.Internal hiding ( Angle )
import Naqsha.Geometry.Angle.Internal         ( Angle )

import Naqsha.Geometry.LatLon.Internal hiding (Latitude, Longitude)
import Naqsha.Geometry.LatLon.Internal        (Latitude, Longitude)


import Naqsha.Geometry.GeoHash


-- $internals$
--
-- The basic types like `Angle`, `Latitude` or `Longitude` are exposed
-- as opaque types from this module. This gives a certain amount of
-- type safety and hence should only use this module. For the rare
-- case when some non-trivial operations are need to be defined, we
-- expose the following internal modules.
--
-- * "Naqsha.Geometry.Angle.Internal"
-- * "Naqsha.Geometry.LatLon.Internal"


-- $latandlong$
--
-- The type `LatLon` provides a way to present points on the globe as
-- a pair of its `Latitude` and `Longitude`.
--
-- == Examples
--
-- > kanpurLatitude  :: Latitude
-- > kanpurLatitude  = lat $ degree 26.4477777
-- > kanpurLongitude :: Longitude
-- > kanpurLongitude = lon $ degree 80.3461111
-- > kanpurGeo       :: LatLon
-- > kanpurGeo       = LatLon kanpurLatitude kanpurLongitude
--
-- You can also specify the latitude and longitude in units of degree,
-- minute and seconds.
--
-- > kanpurLatitude  = lat $ degree 26 <> minute 26 <> second 52
-- > kanpurLongitude = lon $ degree 80 <> minute 20 <> second 46
--
-- The show and read instance of the `Latitude` and `Longitude` types
-- uses degrees for displaying and reading respectively. Show and Read
-- instances can express these quantities up to Nano degree precision.
--
-- == Convention on sign.
--
-- For latitudes, positive means north of the equator and negative
-- means south. In the case of longitudes, positive means east of the
-- longitude zero and negative means west. However, if you find these
-- conventions confusing you can use the combinators `north`, `south`,
-- `east`, and `west` when constructing latitudes or longitudes.
--

-- $geohashing$
--
-- Geometric hashing is a technique of converting geometric
-- coordinates into 1-dimension strings. Often these hashes ensures
-- that string with large common prefix are close by (although not the
-- converse). Hence, these hashes can be used to stored geo-cordinates
-- in database and build into it a sense of location awareness. We support
-- the following geometric hashing:
--
-- ["Naqsha.Geometry.Coordinate.GeoHash":] The geohash standard
-- (<https://en.wikipedia.org/wiki/Geohash>).
--
-- None of these modules are imported by default the user may import
-- the one that is most desirable.

-- $distance$
--
-- Calculating quantities like distance, bearing etc depends on the
-- model of the globe that we choose. Even in a given model we might
-- have different algorithms to compute the distance depending on
-- speed-accuracy trade-offs. Choosing the correct model and
-- algorithms is application dependent and hence we do not expose any
-- default ones. The following modules can be imported depending on the need
--
-- ["Naqsha.Geometry.Spherical": ] Assume a spherical model of the
-- globe. Distance is calculated using the haversine formula.

