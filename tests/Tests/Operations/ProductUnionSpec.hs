module Tests.Operations.ProductUnionSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assert2Fa)
import Operations.Regular (run, productUnion)

spec :: Spec
spec =
  describe "Union of FAs" $
    describe "0.txt and 2.txt" $ do
      
      it "accepts 'a17 a18 a18'" $
        assert2Fa "0.txt" "2.txt" (\fa1 fa2 -> run (fa1 `productUnion` fa2) ["a17", "a18" , "a18"])

      it "accepts 'a a'" $
        assert2Fa "0.txt" "2.txt" (\fa1 fa2 -> run (fa1 `productUnion` fa2) ["a", "b"])

      it "doesn't accept 'a b b'" $
        assert2Fa "0.txt" "2.txt" (\fa1 fa2 -> not $ run (fa1 `productUnion` fa2) ["a", "b", "b"])
