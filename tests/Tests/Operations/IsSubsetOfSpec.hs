module Tests.Operations.IsSubsetOfSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assert2Fa)
import Operations.Regular (isSubsetOf)
import qualified Operations.Antichain.Inclusion as Antichain (isSubsetOf)

spec :: Spec
spec =
  describe "FA" $ do

    it "1.txt is subset of FA 0.txt" $
      assert2Fa "1.txt" "0.txt" isSubsetOf

    it "0.txt is not subset of FA oneStateFa.txt using antichain algorithm" $
      assert2Fa "0.txt" "oneStateFa.txt" (\fa1 fa2 -> not $ Antichain.isSubsetOf fa1 fa2)

    it "1.txt is subset of FA 0.txt using antichain algorithm" $
      assert2Fa "1.txt" "0.txt" Antichain.isSubsetOf
