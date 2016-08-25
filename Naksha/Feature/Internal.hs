{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}

-- | Internal module for features.
module Naksha.Feature.Internal
       (
         -- * Features and FeatureSet
         -- $featureset$
         Feature(..)
       , FeatureSet, fromList, toList, extract
       ) where

import qualified Data.Map       as Map
import           Data.Monoid    hiding ((<>))
import           Data.Semigroup
import           Data.Typeable


-- $featureset$
--
-- An actual map is essentially a set of attributes assigned to
-- objects like geo-locations, trails etc. Th type `Feature` captures
-- these attributes and can have pretty generic any (any instance of
-- `Typeable` is allowed). However, depending on the actual mapping
-- format, some features are meaningless.
--
-- The type `FeatureSet` captures a set of features. It allows for
-- extraction of individual features if it exists.

-- | A feature is nothing but an existentially quantified `Typeable` value.
data  Feature = forall t . (Semigroup t, Typeable t, Show t) => Feature t

instance Semigroup Feature where
  (<>) (Feature t1) (Feature t2) = Feature $ maybe t1 comb mt2
    where mt2  = cast t2 `asTypeOf` Just t1
          comb = (t1<>)

instance Show Feature where
  show (Feature t) = show t

-- | A set of features. Extracting features from this list is fast. Feature sets
-- are monoids where the monoid operations is a right biased union.
newtype FeatureSet = FeatureSet (Map.Map TypeRep Feature)

instance Show FeatureSet where
  show = (++) "fromList " . show . toList

instance Monoid FeatureSet where
  mempty  = FeatureSet Map.empty
  mappend (FeatureSet f1) (FeatureSet f2) = FeatureSet $ Map.unionWith (<>) f1 f2

-- | Construct a feature set out of a set of features.
fromList :: [Feature] -> FeatureSet
fromList   = FeatureSet . Map.fromListWith (<>) . map mkEntry
  where mkEntry f@(Feature t) = (typeOf t, f)

-- | Convert the feature set to a list.
toList :: FeatureSet -> [Feature]
toList (FeatureSet s) = map snd $ Map.toList s

-- | Extract a given feature out of the feature set.
extract :: Typeable t => FeatureSet -> Maybe t
extract = extractP undefined
  where extractP :: Typeable t => t -> FeatureSet -> Maybe t
        extractP t (FeatureSet s) = Map.lookup (typeOf t) s >>= castFeature undefined
        castFeature :: Typeable t => t -> Feature -> Maybe t
        castFeature _ (Feature x) = cast x
