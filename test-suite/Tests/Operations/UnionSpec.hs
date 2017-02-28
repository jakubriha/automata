module Tests.Operations.UnionSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assert2Fa)
import Operations.Regular (charsToSymbols, run, union)

spec :: Spec
spec =
  describe "Union of FAs" $
    describe "oneStateFa.txt and oneStateFa.txt" $
      it "accepts empty string" $
        assert2Fa "oneStateFa.txt" "oneStateFa.txt" (\fa1 fa2 -> run (fa1 `union` fa2) (charsToSymbols ""))

