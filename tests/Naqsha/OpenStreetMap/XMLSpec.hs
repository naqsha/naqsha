module Naqsha.OpenStreetMap.XMLSpec where

import Data.Conduit
import Data.Conduit.List (sourceList)
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck


import Naqsha.OpenStreetMap.XML
import Naqsha.OpenStreetMap.Stream

import Naqsha.Arbitrary

spec :: Spec
spec = describe "xml file" $ do
  prop "osm . compile = id" $ do
    let osmDotCompile evs  = sourceToList $ sourceList evs =$= compileDoc =$= osm
        in forAll genOsmEvents $ \ xs -> osmDotCompile xs `shouldBe` Just xs
  prop "parse . asXML = id" $ do
    let result evs = sourceToList $  asXML (sourceList evs) =$= parse
      in forAll genOsmEvents $ \ evs -> (result evs `shouldReturn` evs)

  prop "parse . asPrettyXML = id" $ do
    let result evs = sourceToList $  asPrettyXML (sourceList evs) =$= parse
      in forAll genOsmEvents $ \ evs -> (result evs `shouldReturn` evs)
