module Tests.Operations.IntersectSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assert2Fwa)
import Operations (charsToLabels, run, intersect)

spec :: Spec
spec =
  describe "Intersection of" $
    describe "Fwa defined in 0.txt and 1.txt" $ do
      it "accepts 'a17 a17 a17 a17 a17'" $
        assert2Fwa (testDirectory ++ "0.txt") (testDirectory ++ "1.txt") (\fwa1 fwa2 -> run (fwa1 `intersect` fwa2) ["a17", "a17", "a17", "a17", "a17"])

      it "doesn't accept 'a18 a18 a18'" $
        assert2Fwa (testDirectory ++ "0.txt") (testDirectory ++ "1.txt") (\fwa1 fwa2 -> not $ run (fwa1 `intersect` fwa2) ["a18", "a18", "a18"])

testDirectory =
  "test-suite/AutomataExamples/"

