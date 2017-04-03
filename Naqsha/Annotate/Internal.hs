{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}

-- | Module to annotate objects with semantic features.
module Naqsha.Annotate.Internal
       ( -- * Annotated objects.
         -- $annotate$

         -- ** Setting attributes, inspecting fields and extracting features.
         --
         -- $extract$
         Annotated, value, annotation, feature, fromIso
       , Attribute, erase, setAttribute, _fromMaybe
       ) where

import           Control.Category
import           Control.Lens
import           Control.Applicative
import qualified Data.Map       as Map
import           Data.Maybe
import           Data.Typeable


import           Prelude        hiding ((.)) -- To avoid redundunt import warnings.

----------------------- Features and Feature set -----------------------------------------------

-- | A feature is nothing but an existentially quantified `Typeable` value.
data Feature = forall t . Typeable t => Feature t

-- | recover the actual feature value.
unFeature :: Typeable t => Feature -> Maybe t
unFeature (Feature x) = cast x


type FeatureMap = Map.Map TypeRep Feature

instance Show Feature where
  show (Feature t) = "<" ++ show (typeOf t) ++ " feature>"

------------------------------------- Annotated elements ---------------------------------------

-- | An annotated type.
data Annotated a = Annotated { __value     :: a
                             , _annotation :: FeatureMap
                             } deriving Show

makeLenses ''Annotated

-- | The lens to focus on the value inside an Annotated object.
value :: Lens' (Annotated a) a
value = _value

instance Functor Annotated where
  fmap f (Annotated a ann) = Annotated (f a) ann

instance Applicative Annotated where
  pure = flip Annotated Map.empty
  (Annotated f _) <*> (Annotated x ann) =  Annotated (f x) ann

-- | An attribute for an annotated type. An attribute is just a lens and hence
-- all the lens operations can be brought into effect here.
type Attribute a v = Lens' (Annotated a) (Maybe v)

-- | Erase a given attribute. The operation @erase attr@ can also be
-- achieved using the lens operation @attr .~ Nothing@.
erase :: Attribute a v -> Annotated a -> Annotated a
erase attr = attr .~ Nothing

-- | Sets a given attribute value. The operation @setAttribute attr v@
-- can be achieved using @attr ?~ v@.
setAttribute :: Attribute a v -> v -> Annotated a -> Annotated a
setAttribute attr v = attr ?~ v


-- | Lens to look into a custom feature of the annotation.
feature :: Typeable t => Attribute a t
feature = annotation . fsLens undefined
  where featureLens :: Typeable t => Lens' (Maybe Feature) (Maybe t)
        featureLens  = lens (>>=unFeature) $ \ s -> maybe s (Just . Feature)
        fsLens      :: Typeable t => t -> Lens' FeatureMap (Maybe t)
        fsLens t     = at (typeOf t) . featureLens

-- | Construct an attribute from
fromIso :: Typeable t => AnIso' t v -> Attribute a v
fromIso isom = feature . mapping isom

-- | Lens that takes a value
_fromMaybe :: t -> Lens' (Maybe t) t
_fromMaybe t = lens (fromMaybe t) $ const Just
