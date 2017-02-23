module Tests.Operations.DeterminizeSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assertFa)
import Operations (charsToSymbols, determinize, run)

spec :: Spec
spec =
  describe "Determinized FA" $ do
    describe "2.txt" $ do

      it "accepts 'a a'" $
        assertFa "2.txt" (\fa -> run (determinize fa) (charsToSymbols "aa"))

      it "accepts 'a b'" $
        assertFa "2.txt" (\fa -> run (determinize fa) (charsToSymbols "ab"))

      it "doesn't accept 'b b'" $
        assertFa "2.txt" (\fa -> not $ run (determinize fa) (charsToSymbols "bb"))

