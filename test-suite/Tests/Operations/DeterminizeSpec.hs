module Tests.Operations.DeterminizeSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assertFa)
import Operations (charsToSymbols, determinize, run)

spec :: Spec
spec =
  describe "Fwa" $ do
    describe "defined in 2.txt" $ do

      it "accepts 'a a' after determinization" $
        assertFa (testDirectory ++ "2.txt") (\fwa -> run (determinize fwa) (charsToSymbols "aa"))

      it "accepts 'a b' after determinization" $
        assertFa (testDirectory ++ "2.txt") (\fwa -> run (determinize fwa) (charsToSymbols "ab"))

      it "doesn't accept 'b b' after determinization" $
        assertFa (testDirectory ++ "2.txt") (\fwa -> not $ run (determinize fwa) (charsToSymbols "bb"))

testDirectory =
  "test-suite/AutomataExamples/"

