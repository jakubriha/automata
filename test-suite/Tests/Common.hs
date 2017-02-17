module Tests.Common
  ( assertFa
  , assert2Fa
  ) where

import Test.Hspec

import Types.Fa (Fa, State, Label)
import Parsing.General (loadFa)

assertFa :: FilePath -> (Fa Label State -> Bool) -> Expectation
assertFa filePath condition =
  loadFa filePath >>= \fa ->
    case fa of
      Left parseError -> expectationFailure parseError
      Right fa -> fa `shouldSatisfy` condition

assert2Fa :: FilePath -> FilePath -> (Fa Label State -> Fa Label State -> Bool) -> Expectation
assert2Fa filePath1 filePath2 condition =
  do firstFa <- loadFa filePath1
     secondFa <- loadFa filePath2
     case firstFa of
       Left parseError -> expectationFailure parseError
       Right fa1 -> case secondFa of
                  Left parseError -> expectationFailure parseError
                  Right fa2 -> fa2 `shouldSatisfy` condition fa1

