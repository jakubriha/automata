module Tests.Operations.DeterminizeSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assertFa)
import Operations (charsToSymbols, determinize, run)

spec :: Spec
spec =
  describe "fa" $ do
    describe "defined in 2.txt" $ do

      it "accepts 'a a' after determinization" $
        assertFa (testDirectory ++ "2.txt") (\fa -> run (determinize fa) (charsToSymbols "aa"))

      it "accepts 'a b' after determinization" $
        assertFa (testDirectory ++ "2.txt") (\fa -> run (determinize fa) (charsToSymbols "ab"))

      it "doesn't accept 'b b' after determinization" $
        assertFa (testDirectory ++ "2.txt") (\fa -> not $ run (determinize fa) (charsToSymbols "bb"))

testDirectory =
  "test-suite/AutomataExamples/"

