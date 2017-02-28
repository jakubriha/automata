module Tests.Operations.IsSubsetOfSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assertFa, assert2Fa)
import Operations.Regular (isSubsetOf)

spec :: Spec
spec =
  describe "FA" $ do

    it "0.txt is subset of FA oneStateFa.txt" $
      assert2Fa "0.txt" "oneStateFa.txt" isSubsetOf

    it "1.txt is subset of FA 0.txt" $
      assert2Fa "1.txt" "0.txt" isSubsetOf

