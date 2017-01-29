module Tests.Operations.UnionSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assert2Fwa)
import Operations (charsToLabels, run, union)

spec :: Spec
spec =
  describe "Union of" $
    describe "Fwa defined in oneStateFwa.txt and oneStateFwa.txt" $
      it "accepts empty language" $
        assert2Fwa (testDirectory ++ "oneStateFwa.txt") (testDirectory ++ "oneStateFwa.txt") (\fwa1 fwa2 -> run (fwa1 `union` fwa2) (charsToLabels ""))

testDirectory =
  "test-suite/AutomataExamples/"

