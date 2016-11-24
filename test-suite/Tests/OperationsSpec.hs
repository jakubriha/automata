module Tests.OperationsSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assertFwa)
import Parsing.General (loadAndParseFwa)
import Operations (charsToLabels, run)

spec :: Spec
spec =
  describe "Fwa" $ do
    describe "accepting ''" $ do

      it "accepts empty language" $
        assertFwa (testDirectory ++ "oneStateFwa.txt") (\fwa -> run fwa [])

      it "doesn't accept non-empty language" $
        assertFwa (testDirectory ++ "oneStateFwa.txt") (\fwa -> not $ run fwa (charsToLabels "Hello World!"))

    describe "accepting 0.txt" $ do

      it "accepts 'a17 a18 a18 a2'" $
        assertFwa (testDirectory ++ "0.txt") (\fwa -> run fwa ["a17", "a18", "a18", "a2"])

      it "doesn't accept 'a18 a18 a2'" $
        assertFwa (testDirectory ++ "0.txt") (\fwa -> not $ run fwa ["a18", "a18", "a2"])

testDirectory =
  "test-suite/AutomataExamples/"

