module Tests.Parsing.GeneralSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assertFa)
import Parsing.General
import Types.Fa as Fa

spec :: Spec
spec = do
  describe "Fa" $ do

    it "is parsed from file 0.txt" $ do
      canParseFa "0.txt"

    it "has correct state count in file oneStateFa.txt" $ do
      assertFa "oneStateFa.txt" ((== 1) . length . Fa.states)

    it "has correct state count in file 0.txt" $ do
      assertFa "0.txt" ((== 4) . length . Fa.states)

    it "has correct transition count in file 0.txt" $ do
      assertFa "0.txt" ((== 13) . length . Fa.transitions)

canParseFa :: FilePath -> Expectation
canParseFa filePath =
  assertFa filePath (const True)

