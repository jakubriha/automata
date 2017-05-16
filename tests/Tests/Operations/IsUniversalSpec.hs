module Tests.Operations.IsUniversalSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assertFa)
import Operations.Regular (charsToSymbols)
import qualified Operations.Antichain.Universality as Antichain (isUniversal)

spec :: Spec
spec =
  describe "FA" $ do

    it "oneStateFa.txt is universal using antichain algorithm" $
      assertFa "oneStateFa.txt" Antichain.isUniversal

    it "4.txt is universal using antichain algorithm" $
      assertFa "4.txt" Antichain.isUniversal

    it "0.txt is not universal using antichain algorithm" $
      assertFa "0.txt" (not . Antichain.isUniversal)
