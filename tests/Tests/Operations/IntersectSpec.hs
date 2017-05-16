module Tests.Operations.IntersectSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assert2Fa)
import Operations.Regular (charsToSymbols, run, intersect)

spec :: Spec
spec =
  describe "Intersection of FAs" $
    describe "0.txt and 1.txt" $ do
      it "accepts 'a17 a17 a17 a17 a17'" $
        assert2Fa "0.txt" "1.txt" (\fa1 fa2 -> run (fa1 `intersect` fa2) ["a17", "a17", "a17", "a17", "a17"])

      it "doesn't accept 'a18 a18 a18'" $
        assert2Fa "0.txt" "1.txt" (\fa1 fa2 -> not $ run (fa1 `intersect` fa2) ["a18", "a18", "a18"])
