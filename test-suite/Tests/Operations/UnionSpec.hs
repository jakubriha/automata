module Tests.Operations.UnionSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assert2Fa)
import Operations (charsToLabels, run, union)

spec :: Spec
spec =
  describe "Union of" $
    describe "Fa defined in oneStateFa.txt and oneStateFa.txt" $
      it "accepts empty language" $
        assert2Fa (testDirectory ++ "oneStateFa.txt") (testDirectory ++ "oneStateFa.txt") (\fa1 fa2 -> run (fa1 `union` fa2) (charsToLabels ""))

testDirectory =
  "test-suite/AutomataExamples/"

