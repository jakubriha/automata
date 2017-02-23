module Tests.Operations.RunSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assertFa)
import Operations (charsToSymbols, run)

spec :: Spec
spec =
  describe "FA" $ do
    describe "oneStateFa.txt" $ do

      it "accepts empty language" $
        assertFa "oneStateFa.txt" (\fa -> run fa [])

      it "doesn't accept non-empty string" $
        assertFa "oneStateFa.txt" (\fa -> not $ run fa (charsToSymbols "Hello World!"))

    describe "0.txt" $ do

      it "accepts 'a17 a18 a18 a2'" $
        assertFa "0.txt" (\fa -> run fa ["a17", "a18", "a18", "a2"])

      it "doesn't accept 'a18 a18 a2'" $
        assertFa "0.txt" (\fa -> not $ run fa ["a18", "a18", "a2"])

