{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}

-- | Module to annotate objects with semantic features.
module Naksha.Annotate
       ( -- * Annotated objects.
         -- $annotate$


         -- ** Setting attributes, inspecting fields and extracting features.
         --
         -- $extract$
         Annotated, annotate, unAnnotate, (.=), (.>), extract
       , IsFeature

       ) where

import           Control.Applicative
import qualified Data.Map       as Map
import           Data.Monoid    hiding ((<>))
import           Data.Semigroup
import           Data.Typeable



-- $annotate$
--
-- Geographic objects like geo-positions, trails, routes etc in actual
-- maps comes with annotations. Such objects are captured by the type
-- `Annotated`.  An object can be annotated with any type that is an
-- constrained by the constraint type `IsFeature`. It is required to
-- be:
--
-- [Typeable:] Required for efficient storing and extracting from
-- feature sets and extracting when required
--
-- [Semigroup:] When an object is annotated with the same feature type
-- multiple times, the semi-group instance is used to summarise the
-- multiple annotations.
--
-- [Show:] Features should be printable.

-- $extract$
--
-- We disallow direct access to the feature set of an annotated
-- object. However we can work with the annotations as follows.
--
-- 1. An object is annotated using the idiom
--
-- >
-- > annotate x [attr1 .= v2, attr2 .= v2]
-- >
--
-- where each of @attr1@, @attr2@ are _attributes_ to be set. For a
-- feature type @t@, any function of type @v -> t@ is an attribute
-- that takes a value @v@ and sets the feature @t@.
--
-- 2. One can extract any _field_ of an annotated object using the
--    idiom
--
-- >
-- > a .> field
-- >
--
-- A field is any function from @t -> a@ where t is a feature.
--
-- 3. One can use the function `extract` to actually extract a feature.
--


------------------------------------- Annotated elements ---------------------------------------

-- | An annotated type.
data Annotated a = Annotated { unAnnotate :: a
                             , featureSet :: FeatureSet
                             } deriving Show

-- | An @v@ valued attribute is nothing but a function from @v@ to some feature @t@. This operator
-- simulates the assignment of an attribute.
(.=) :: IsFeature t
     => (v -> t)        -- ^ attribute
     -> v               -- ^ value to set for the attribute
     -> FeatureSet
(.=) attr v = FeatureSet $ Map.singleton key $ Feature ftr
  where key = typeOf $ ftr
        ftr = attr v

-- | Annotate an element with the given feature sets.
annotate :: a -> [FeatureSet] -> Annotated a
annotate a = Annotated a . mconcat

-- | Recover a feature from an annotated element if set.
extract :: IsFeature t => Annotated a -> Maybe t
extract = extractFrom . featureSet

-- | Let @t@ be a feature type, then a field of a feature is an arbitrary function from @t -> a@. This
-- operator can be used to extract a field of a
(.>) :: IsFeature t => Annotated a -> (t -> b) -> Maybe b
(.>) ann field = field <$> extract ann

----------------------- Features and Feature set -----------------------------------------------

-- | Constraint type capturing types that are allowed to be used as
-- features.
type IsFeature t = (Semigroup t, Typeable t, Show t)

-- | A feature is nothing but an existentially quantified `Typeable` value.
data  Feature = forall t . IsFeature t => Feature t

instance Show Feature where
  show (Feature t) = show t

-- | A set of features. Extracting features from this list is fast. Feature sets
-- are monoids where the monoid operation combines the set of
-- features.
newtype FeatureSet = FeatureSet (Map.Map TypeRep Feature)

instance Show FeatureSet where
  show (FeatureSet fs) = show $ Map.elems fs

instance Semigroup Feature where
  (<>) (Feature t1) (Feature t2) = Feature $ maybe t1 comb mt2
    where mt2  = cast t2 `asTypeOf` Just t1
          comb = (t1<>)

instance Semigroup FeatureSet where
  (<>) = mappend

instance Monoid FeatureSet where
  mempty  = FeatureSet Map.empty
  mappend (FeatureSet f1) (FeatureSet f2) = FeatureSet $ Map.unionWith (<>) f1 f2


-- | Extract a given feature out of the feature set.
extractFrom :: IsFeature t => FeatureSet -> Maybe t
extractFrom = extractFromP undefined
  where extractFromP :: Typeable t => t -> FeatureSet -> Maybe t
        extractFromP t (FeatureSet s) = Map.lookup (typeOf t) s >>= castFeature undefined
        castFeature :: Typeable t => t -> Feature -> Maybe t
        castFeature _ (Feature x) = cast x
