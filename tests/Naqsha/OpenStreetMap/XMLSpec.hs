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
  prop "translate . compile = id" $ do
    let transComp evs  = sourceToList $ sourceList evs =$= compile =$= translate
        in forAll genOsmEvents $ \ xs -> transComp xs `shouldBe` Just xs
