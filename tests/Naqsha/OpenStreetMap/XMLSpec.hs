module Naqsha.OpenStreetMap.XMLSpec where

import Data.Conduit
import Data.Conduit.List (sourceList)
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck


import Naqsha.OpenStreetMap.XML
import Naqsha.Arbitrary

spec :: Spec
spec = describe "xml file" $ do
  prop "osm . compile = id" $ do
    let osmDotCompile evs  = sourceToList $ sourceList evs =$= compile =$= osm
        in forAll genOsmEvents $ \ xs -> osmDotCompile xs `shouldBe` Just xs
