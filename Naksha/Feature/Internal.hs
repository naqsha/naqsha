{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}

-- | Internal module for features.
module Naksha.Feature.Internal
       ( Feature(..)
       , FeatureSet, fromList, toList, extract
       ) where

import qualified Data.Map       as Map
import           Data.Monoid
import           Data.Typeable

-- | A feature is nothing but an existentially quantified `Typeable` value.
data  Feature = forall t . Typeable t => Feature t

-- | A set of features. Extracting features from this list is fast. Feature sets
-- are monoids where the monoid operations is a right biased union.
newtype FeatureSet = FeatureSet (Map.Map TypeRep Feature)

instance Monoid FeatureSet where
  mempty  = FeatureSet Map.empty
  mappend (FeatureSet f1) (FeatureSet f2) = FeatureSet $ Map.unionWith sel2nd f1 f2
    where sel2nd = flip const

-- | Construct a feature set out of a set of features.
fromList :: [Feature] -> FeatureSet
fromList   = FeatureSet . Map.fromList . map mkEntry
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
