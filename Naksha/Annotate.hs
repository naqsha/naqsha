{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}

-- | Module to annotate objects with semantic features.
module Naksha.Annotate
       ( -- * Annotated objects.
         -- $annotate$

         -- ** Setting attributes, inspecting fields and extracting features.
         --
         -- $extract$
         Annotated, value, annotation, feature
       , AttributeLens, (@=), (@?)
       , IsFeature
       ) where

import           Control.Category
import           Control.Lens
import           Control.Applicative
import qualified Data.Map       as Map
import           Data.Typeable


import           Prelude        hiding ((.)) -- To avoid redundunt import warnings.


-- $annotate$
--
-- Geographic objects like geo-positions, trails, routes etc in actual
-- maps comes with annotations. Annotations on an object is captured
-- by the `Annotation` type and an annotated object of type @a@ is
-- captured by the the type @`Annotated` a@. Besides standard
-- annotations the library supports annotation by any custom feature
-- type. The only restriction is that it be an instance of
-- `IsFeature`.

----------------------- Features and Feature set -----------------------------------------------

-- | Constraint type capturing types that are allowed to be used as
-- features.
type IsFeature t = (Typeable t, Show t)

-- | A feature is nothing but an existentially quantified `Typeable` value.
data Feature = forall t . IsFeature t => Feature t

type FeatureMap = Map.Map TypeRep Feature

instance Show Feature where
  show (Feature t) = show t

------------------------------------- Annotated elements ---------------------------------------

-- | An annotated type.
data Annotated a = Annotated { _value      :: a
                             , _annotation :: FeatureMap
                             } deriving Show



makeLenses ''Annotated

instance Functor Annotated where
  fmap f (Annotated a ann) = Annotated (f a) ann

instance Applicative Annotated where
  pure = flip Annotated Map.empty
  (Annotated f _) <*> (Annotated x ann) =  Annotated (f x) ann


type AttributeLens a v = Lens' (Annotated a) (Maybe v)

-- | Lens to look into a custom feature of the annotation.
feature :: IsFeature t => AttributeLens a t
feature = annotation . featureSetLens
  where castFeature (Just (Feature x)) = cast x
        castFeature _                  = Nothing

        featureLens :: IsFeature t => Lens' (Maybe Feature) (Maybe t)
        featureLens = lens castFeature $ \ s -> maybe s (Just . Feature)
        featureSetLens = fsLens undefined
          where fsLens :: IsFeature t => t -> Lens' FeatureMap (Maybe t)
                fsLens t = at (typeOf t) . featureLens


-- | Set a custom feature.
(@=) :: IsFeature t
     => Iso' t v      -- ^ the attribute to set (ig
     -> v             -- ^ the attribute value
     -> Annotated a
     -> Annotated a
(@=) isom v =  feature .~ Just (view (from isom) v)

-- | Query a custom feature.
(@?) :: IsFeature t => Annotated a -> Lens' t v -> Maybe v
annA @? lnz = annA ^? feature . _Just . lnz
