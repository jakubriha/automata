module Tests.Parsing.GeneralSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck

import Parsing.General
import Types.Fwa as Fwa

spec :: Spec
spec = do
  describe "Fwa" $ do
    
    it "is parsed from file 1.txt" $ do
      canParseFwa (head testFiles)

    it "has correct state count in file 1.txt" $ do
      assertFwa (head testFiles) (\fwa -> length (Fwa.states fwa) == 4)

    it "has correct transition count in file 1.txt" $ do
      assertFwa (head testFiles) (\fwa -> length (Fwa.transitions fwa) == 14)    

assertFwa :: FilePath -> (Fwa -> Bool) -> Expectation
assertFwa filePath condition =
  loadAndParseFwa filePath >>= \fwa ->
    case fwa of
      Left parseError -> expectationFailure parseError
      Right fwa -> fwa `shouldSatisfy` condition

canParseFwa :: FilePath -> Expectation
canParseFwa filePath =
  assertFwa filePath (const True)

testFiles =
  fmap ("test-suite/AutomataExamples/" ++) ["0.txt", "1.txt", "2.txt", "3.txt", "4.txt"]
