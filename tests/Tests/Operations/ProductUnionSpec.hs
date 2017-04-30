module Tests.Operations.ProductUnionSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assert2Fa)
import Operations.Regular (run)
import Operations.Product (union)

spec :: Spec
spec =
  describe "Product union of FAs" $ do
    describe "oneStateFa.txt and oneStateFa.txt" $ do
      
      it "accepts empty string" $
        assert2Fa "oneStateFa.txt" "oneStateFa.txt" (\fa1 fa2 -> run (fa1 `union` fa2) [])

    describe "0.txt and 2.txt" $ do
      
      it "accepts 'a17 a17 a18 a0'" $
        assert2Fa "0.txt" "2.txt" (\fa1 fa2 -> run (fa1 `union` fa2) ["a17", "a17", "a18", "a0"])

      it "accepts 'a a'" $
        assert2Fa "0.txt" "2.txt" (\fa1 fa2 -> run (fa1 `union` fa2) ["a", "a"])

      it "doesn't accept 'b b'" $
        assert2Fa "0.txt" "2.txt" (\fa1 fa2 -> not $ run (fa1 `union` fa2) ["b", "b"])
