module Tests.Common
  ( assertFwa
  ) where

import Test.Hspec

import Types.Fwa (Fwa)
import Parsing.General (loadAndParseFwa)

assertFwa :: FilePath -> (Fwa -> Bool) -> Expectation
assertFwa filePath condition =
  loadAndParseFwa filePath >>= \fwa ->
    case fwa of
      Left parseError -> expectationFailure parseError
      Right fwa -> fwa `shouldSatisfy` condition

