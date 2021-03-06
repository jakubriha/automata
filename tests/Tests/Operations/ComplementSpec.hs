module Tests.Operations.ComplementSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assertFa)
import Operations.Regular (charsToSymbols, complement, run)

spec :: Spec
spec =
  describe "Complement of FA" $ do
    describe "0.txt" $ do

      it "accepts 'a0 a0'" $
        assertFa "0.txt" (\fa -> run (complement fa) ["a0", "a0"])

      it "doesn't accept 'a17 a17 a17'" $
        assertFa "0.txt" (\fa -> not $ run (complement fa) ["a17", "a17", "a17"])
