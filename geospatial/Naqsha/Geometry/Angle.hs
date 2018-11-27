-- | Basic types associated with geometry.
module Naqsha.Geometry.Angle
  ( Angle
  , degree , minute, second
  , radian
  , toDegree, toRadian
  , Angular(..)
  ) where

import           Naqsha.Geometry.Internal

------------------------------ The angular class ------------------------

-- | Angular quantities.
class Angular a where
  toAngle   :: a -> Angle

instance Angular Angle where
  toAngle = id


instance Angular Latitude where
  toAngle = unLat

instance Angular Longitude where
  toAngle = unLong
