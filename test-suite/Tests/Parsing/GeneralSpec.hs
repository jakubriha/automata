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
      canParseFa (head testFiles)

    it "has correct state count in file oneStateFa.txt" $ do
      assertFa "test-suite/AutomataExamples/oneStateFa.txt" ((== 1) . length . Fa.states)

    it "has correct state count in file 0.txt" $ do
      assertFa (head testFiles) ((== 4) . length . Fa.states)

    it "has correct transition count in file 0.txt" $ do
      assertFa (head testFiles) ((== 13) . length . Fa.transitions)

canParseFa :: FilePath -> Expectation
canParseFa filePath =
  assertFa filePath (const True)

testFiles =
  fmap ("test-suite/AutomataExamples/" ++) ["0.txt", "1.txt", "2.txt", "3.txt", "4.txt"]
