-- | This module captures

module Naqsha.Annotate
       ( -- * Annotated values.
         -- $annotate$
         Annotated, value, feature

         -- ** Some common features
       , Name, MultiName, Description, Comment, LinkSet
       , Elevation

       -- * Attribute
       , Attribute, erase, setAttribute
       , module Naqsha.Annotate.Attribute
       ) where

import Naqsha.Annotate.Internal
import Naqsha.Annotate.Feature
import Naqsha.Annotate.Attribute

-- $annotate$
--
-- Geographic objects like geo-positions, trails, routes etc in actual
-- maps comes with annotations. An annotated value of type @a@ is
-- captured by the the type @`Annotated` a@. An object can be
-- annotated with a set of features. Any instance of the class
-- Typeable can be used as features for annotation.

-- $attributes$
--
-- Attributes of an annotated value give access to the features of the
-- value. The type `Attribute` is just a lens and can be used to
-- get,set, update and erase.
