module Tests.Operations.RunSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assertFa)
import Operations (charsToLabels, run)

spec :: Spec
spec =
  describe "Fa" $ do
    describe "defined in oneStateFa.txt" $ do

      it "accepts empty language" $
        assertFa (testDirectory ++ "oneStateFa.txt") (\fa -> run fa [])

      it "doesn't accept non-empty language" $
        assertFa (testDirectory ++ "oneStateFa.txt") (\fa -> not $ run fa (charsToLabels "Hello World!"))

    describe "defined in 0.txt" $ do

      it "accepts 'a17 a18 a18 a2'" $
        assertFa (testDirectory ++ "0.txt") (\fa -> run fa ["a17", "a18", "a18", "a2"])

      it "doesn't accept 'a18 a18 a2'" $
        assertFa (testDirectory ++ "0.txt") (\fa -> not $ run fa ["a18", "a18", "a2"])

testDirectory =
  "test-suite/AutomataExamples/"

