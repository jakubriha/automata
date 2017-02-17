module Tests.Common
  ( assertFwa
  , assert2Fwa
  ) where

import Test.Hspec

import Types.Fwa (Fwa, State, Label)
import Parsing.General (loadFwa)

assertFwa :: FilePath -> (Fwa Label State -> Bool) -> Expectation
assertFwa filePath condition =
  loadFwa filePath >>= \fwa ->
    case fwa of
      Left parseError -> expectationFailure parseError
      Right fwa -> fwa `shouldSatisfy` condition

assert2Fwa :: FilePath -> FilePath -> (Fwa Label State -> Fwa Label State -> Bool) -> Expectation
assert2Fwa filePath1 filePath2 condition =
  do firstFwa <- loadFwa filePath1
     secondFwa <- loadFwa filePath2
     case firstFwa of
       Left parseError -> expectationFailure parseError
       Right fwa1 -> case secondFwa of
                  Left parseError -> expectationFailure parseError
                  Right fwa2 -> fwa2 `shouldSatisfy` condition fwa1

