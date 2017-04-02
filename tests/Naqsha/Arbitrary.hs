{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Naqsha.Arbitrary where

import           Control.Lens
import           Control.Monad.State
import           Data.Conduit                as Conduit
import           Data.Conduit.Combinators    as Conduit
import           Data.Default

import qualified Data.HashMap.Lazy           as HM
import           Data.Text                      (Text, pack)
import           Data.Time
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as VU
import           Test.QuickCheck


import Naqsha.Position
import Naqsha.OpenStreetMap

instance Arbitrary Angle where
  arbitrary = toEnum <$> arbitrary

instance Arbitrary Latitude where
  arbitrary = lat <$> arbitrary

instance Arbitrary Longitude where
  arbitrary = lon <$> arbitrary


instance Arbitrary Geo where
  arbitrary = Geo <$> arbitrary <*> arbitrary

instance Arbitrary GeoBounds where
  arbitrary = toGen def $ do setArbitrary maxLatitude
                             setArbitrary maxLongitude
                             setArbitrary minLatitude
                             setArbitrary minLongitude

instance Arbitrary (OsmID a) where
  arbitrary = OsmID <$> arbitrary


genOsmEvents :: Gen [OsmEvent]
genOsmEvents = oneof [nodeGen, relationGen, wayGen, boundsGen]
    where nodeGen     = eventsFor (undefined :: Osm Node)
          relationGen = eventsFor (undefined :: Osm Relation)
          wayGen      = eventsFor (undefined :: Osm Way)
          boundsGen   = eventsFor (undefined :: GeoBounds)

          singleElement :: Arbitrary a => a -> Conduit.Producer Gen a
          singleElement _ = Conduit.replicateM 1 arbitrary

          eventsFor :: (Arbitrary a, OsmEventElement a) => a -> Gen [OsmEvent]
          eventsFor a = sourceToList $ singleElement a =$= Conduit.awaitForever (toProducer . toSource)


instance Arbitrary (OsmMeta a) where
  arbitrary = toGen def $ do setArbitrary       _osmID
                             genAndSet genUser  _modifiedUser
                             setArbitrary       _modifiedUserID
                             setArbitrary       _timeStamp
                             setArbitrary       _isVisible
                             setArbitrary       _changeSet
                             setArbitrary       _version
    where genUser = genMaybe genText

instance (Arbitrary e, Default e) => Arbitrary (Osm e) where
  arbitrary = toGen def $ do setArbitrary untagged
                             setArbitrary meta
                             genAndSet genTags tags


instance Arbitrary Member where
  arbitrary = oneof [nodeMemberGen, wayMemberGen, relationGen]
    where nodeMemberGen = NodeM     <$> genText <*> arbitrary
          wayMemberGen  = WayM      <$> genText <*> arbitrary
          relationGen   = RelationM <$> genText <*> arbitrary

instance Arbitrary Relation where
  arbitrary = toGen def $ genAndSet (V.fromList <$> listOf arbitrary) relationMembers

instance Arbitrary Way      where
  arbitrary = toGen def $ genAndSet (VU.fromList <$> listOf arbitrary) wayNodes

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary DiffTime where
  arbitrary = picosecondsToDiffTime <$> choose(0, pSecsPerDay)
    where pSecsPerDay = (24 * 3600 - 1) * 10 ^ (12 :: Int)

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> arbitrary

--------------------- Helper functions ------------------------------------

genText :: Gen Text
genText = pack <$> listOf arbitrary


genText1 :: Gen Text
genText1 = pack <$> listOf1 arbitrary


genMaybe :: Gen a -> Gen (Maybe a)
genMaybe gen = oneof [ Just <$> gen , return Nothing ]


genTags :: Gen OsmTags
genTags = HM.fromList <$> listOf entry
  where entry = (,) <$> genText <*> genText

type GenSetter s = StateT s Gen ()

-- | Generate from the setter.
toGen :: s -> GenSetter s -> Gen s
toGen = flip execStateT


setArbitrary :: Arbitrary a
             => Setter' s a
             -> GenSetter s
setArbitrary = genAndSet arbitrary


genAndSet :: Gen a
          -> Setter' s a
          -> GenSetter s
genAndSet g setter = do a <- lift g
                        setter .= a
