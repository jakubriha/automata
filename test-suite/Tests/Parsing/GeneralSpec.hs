module Tests.Parsing.GeneralSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Tests.Common (assertFwa)
import Parsing.General
import Types.Fwa as Fwa

spec :: Spec
spec = do
  describe "Fwa" $ do

    it "is parsed from file 0.txt" $ do
      canParseFwa (head testFiles)

    it "has correct state count in file oneStateFwa.txt" $ do
      assertFwa "test-suite/AutomataExamples/oneStateFwa.txt" (\fwa -> length (Fwa.states fwa) == 1)

    it "has correct state count in file 0.txt" $ do
      assertFwa (head testFiles) (\fwa -> length (Fwa.states fwa) == 4)

    it "has correct transition count in file 0.txt" $ do
      assertFwa (head testFiles) (\fwa -> length (Fwa.transitions fwa) == 13)

canParseFwa :: FilePath -> Expectation
canParseFwa filePath =
  assertFwa filePath (const True)

testFiles =
  fmap ("test-suite/AutomataExamples/" ++) ["0.txt", "1.txt", "2.txt", "3.txt", "4.txt"]
