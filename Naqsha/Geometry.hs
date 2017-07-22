-- | The geometric types and values exposed by naqsha.
module Naqsha.Geometry
       ( module Naqsha.Geometry.Coordinate

       -- ** Angles and angular quantities.
       , module Naqsha.Geometry.Angle

       -- * Geometric hashing.
       -- $geohashing$

       -- * Distance calculation.
       --
       -- $distance$
       --

       -- * Internal details
       -- $internals$
       ) where

import Naqsha.Geometry.Angle
import Naqsha.Geometry.Coordinate

-- Nothing imported here. Only for docs.
import Naqsha.Geometry.Spherical()


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


-- $internals$
--
-- The basic types like `Latitude` or `Longitude` are exposed as
-- opaque types from this module. For type safety, we encourage the
-- users to use this module mostly when dealing with those times. For
-- the rare case when some non-trivial operations need to be defined,
-- we expose the internal module "Naqsha.Geometry.Internal". However,
-- use this interface with caution.
